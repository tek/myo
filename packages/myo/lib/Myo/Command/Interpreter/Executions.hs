module Myo.Command.Interpreter.Executions where

import qualified Chronos
import qualified Control.Lens as Lens
import Control.Lens (mapMOf)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import qualified Data.Map.Strict as Map
import Exon (exon)
import qualified Log
import Polysemy.Chronos (ChronosTime)
import Polysemy.Process (Pid)
import qualified Time
import Time (MilliSeconds (MilliSeconds), while)

import Myo.Command.Data.ActiveTarget (ActiveTarget (ActiveCommand, ActiveCommandShell))
import qualified Myo.Command.Data.Execution as Execution
import Myo.Command.Data.Execution (Execution (Execution), ExecutionSync (ExecutionSync), sync, wait)
import qualified Myo.Command.Data.ExecutionState as ExecutionState
import Myo.Command.Data.ExecutionState (ExecutionState (Pending, Stopped, Tracked, Unknown))
import Myo.Command.Data.UiTarget (UiTarget)
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions (..))
import Myo.Data.CommandId (CommandId, commandIdText)
import Myo.Data.ProcError (ProcError)
import Myo.Effect.MState (MState, mmodify, mreads, mstate, mtrans)
import qualified Myo.Effect.Proc as Proc
import Myo.Effect.Proc (Proc)
import Myo.Interpreter.MState (interpretMState)

type ExecutionsData =
  (Map CommandId Execution, Map UiTarget ActiveTarget)

type ExecutionsState =
  MState ExecutionsData

newExecution ::
  CommandId ->
  Maybe UiTarget ->
  Chronos.Time ->
  MVar () ->
  MVar () ->
  Execution
newExecution i target now wait kill =
  Execution i target ExecutionState.Pending now (ExecutionSync wait kill)

insertTarget :: CommandId -> Bool -> UiTarget -> Map UiTarget ActiveTarget -> Map UiTarget ActiveTarget
insertTarget i cmdShell target =
  Map.alter (Just . alter) target
  where
    alter = \case
      Just (ActiveCommandShell sid activeTarget) ->
        ActiveCommandShell sid (Just (alter activeTarget))
      Just (ActiveCommand _) ->
        new
      Nothing ->
        new
    new =
      if cmdShell then ActiveCommandShell i Nothing else ActiveCommand i

activeShell ::
  Map UiTarget ActiveTarget ->
  UiTarget ->
  Maybe CommandId
activeShell ts target =
  extract (Map.lookup target ts)
  where
    extract = \case
      Just (ActiveCommandShell i t) ->
        Just (fromMaybe i (extract t))
      Just (ActiveCommand _) ->
        Nothing
      Nothing ->
        Nothing

readTargetCommand ::
  Member ExecutionsState r =>
  UiTarget ->
  Sem r (Maybe CommandId)
readTargetCommand target = do
  mreads (extract . Map.lookup target . snd)
  where
    extract = \case
      Just (ActiveCommandShell _ t) ->
        extract t
      Just (ActiveCommand i) ->
        Just i
      Nothing ->
        Nothing

insertExecution ::
  CommandId ->
  Bool ->
  Maybe UiTarget ->
  Chronos.Time ->
  MVar () ->
  MVar () ->
  ExecutionsData ->
  (ExecutionsData, Maybe CommandId)
insertExecution i cmdShell target now wait kill (exes, ts) =
  ((newExes, newTs), target >>= activeShell ts)
  where
    newExes =
      Map.insert i (newExecution i target now wait kill) exes
    newTs =
      foldMap (insertTarget i cmdShell) target ts

stopExecution ::
  CommandId ->
  ExecutionsData ->
  (ExecutionsData, Maybe (MVar ()))
stopExecution i (exes, ts) =
  ((newExes, newTs), waitVar)
  where
    newTs =
      maybe ts removeTarget tgt
    ((waitVar, tgt), newExes) = Map.alterF alter i exes
    alter = \case
      Nothing ->
        ((Nothing, Nothing), Nothing)
      Just e@Execution {target, sync = ExecutionSync {wait}} ->
        ((Just wait, target), Just (e & #state .~ ExecutionState.Stopped))
    removeTarget t =
      Map.alter remove t ts
    remove = \case
      Just (ActiveCommandShell sid _) | sid == i ->
        Nothing
      Just (ActiveCommandShell sid t) ->
        Just (ActiveCommandShell sid (remove t))
      Just (ActiveCommand cid) | cid == i ->
        Nothing
      Just (ActiveCommand cid) ->
        Just (ActiveCommand cid)
      Nothing ->
        Nothing

getExecution ::
  Member ExecutionsState r =>
  CommandId ->
  Sem r (Maybe Execution)
getExecution i =
  mreads (Map.lookup i . fst)

withExecution ::
  Member ExecutionsState r =>
  CommandId ->
  (Execution -> Sem r a) ->
  Sem r (Maybe a)
withExecution i f =
  traverse f =<< getExecution i

withExecution_ ::
  Member ExecutionsState r =>
  CommandId ->
  (Execution -> Sem r a) ->
  Sem r ()
withExecution_ i f =
  void (withExecution i f)

modifyExecution ::
  Member ExecutionsState r =>
  CommandId ->
  (Execution -> Sem r Execution) ->
  Sem r ()
modifyExecution cid f =
  mtrans (mapMOf (_1 . at cid . Lens._Just) f)

getState ::
  Member ExecutionsState r =>
  CommandId ->
  Sem r (Maybe ExecutionState)
getState i =
  fmap (.state) <$> getExecution i

running ::
  Member (Proc !! ProcError) r =>
  ExecutionState ->
  Sem r Bool
running = \case
  Tracked pid ->
    False <! Proc.exists pid
  ExecutionState.Running ->
    pure True
  Pending ->
    pure False
  Unknown ->
    pure False
  Stopped ->
    pure False

-- | Wait for a process's children to settle.
-- Read the children, wait for 100ms and query again.
-- If the process has the same single child before and after waiting, return it.
-- If this check failed for five times, return the potential first child.
commandPid ::
  Members [Proc, ChronosTime] r =>
  Pid ->
  Sem r (Maybe Pid)
commandPid parent =
  spin (1 :: Int) =<< Proc.childPids parent
  where
    spin n children = do
      Time.sleep (MilliSeconds 100)
      check n children =<< Proc.childPids parent
    check _ [old] [new] | old == new =
      pure (Just new)
    check n _ new | n >= 5 =
      pure (head new)
    check n _ new =
      spin (n + 1) new

checkState ::
  Member ExecutionsState r =>
  (ExecutionState -> Sem r Bool) ->
  CommandId ->
  Sem r Bool
checkState f i =
  fmap (fromMaybe False) . traverse f =<< getState i

interpretExecutions ::
  Members [Proc !! ProcError, ChronosTime, Log, Resource, Race, Mask, Embed IO] r =>
  InterpreterFor Executions r
interpretExecutions =
  interpretMState (mempty :: ExecutionsData) .
  reinterpret \case
    Get i ->
      getExecution i
    Modify i f ->
      mmodify (first (Map.adjust (#state %~ f) i))
    Start i cmdShell target -> do
      Log.debug [exon|Starting execution '#{commandIdText i}'|]
      now <- Time.now
      wait <- embed newEmptyMVar
      kill <- embed newEmptyMVar
      mstate (insertExecution i cmdShell target now wait kill)
    Executions.Stop i -> do
      waitVar <- mstate (stopExecution i)
      for_ waitVar \ var ->
        embed (tryPutMVar var ())
    Running i ->
      checkState running i
    ActiveTarget target ->
      runMaybeT do
        i <- MaybeT (readTargetCommand target)
        guard =<< lift (checkState running i)
        pure i
    Wait i ->
      withExecution_ i \ e -> embed (readMVar (e ^. #sync . #wait))
    Terminate i ->
      withExecution_ i \ e -> embed (tryPutMVar (e ^. #sync . #kill) ())
    WaitTerminate i -> do
      withExecution_ i \ e -> do
        Log.debug [exon|Waiting for terminate signal for execution '#{commandIdText i}'|]
        embed (readMVar (e ^. #sync . #kill))
      withExecution_ i \ e -> do
        Log.debug [exon|Terminating process for execution '#{commandIdText i}'|]
        for_ (Execution.pid e) (resume_ . Proc.term)
    ShellCommandPid shellPid ->
      resumeAs @_ @Proc Nothing (commandPid shellPid)
    PollPid i ->
      getState i >>= \case
        Just (Tracked pid) ->
          while (MilliSeconds 500) do
            Proc.exists pid !! \ e -> do
              Log.debug [exon|Error when polling pid of '#{commandIdText i}': #{show e}|]
              pure False
        _ -> unit
