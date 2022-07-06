module Myo.Command.Interpreter.Executions where

import Chiasma.Data.Ident (Ident, identText)
import qualified Control.Lens as Lens
import Control.Lens (mapMOf)
import qualified Data.Map.Strict as Map
import Exon (exon)
import qualified Log
import Polysemy.Chronos (ChronosTime)
import qualified Time

import qualified Myo.Command.Data.Execution as Execution
import Myo.Command.Data.Execution (Execution (Execution), ExecutionSync (ExecutionSync), state, sync, wait)
import qualified Myo.Command.Data.ExecutionState as ExecutionState
import Myo.Command.Data.ExecutionState (ExecutionState (..))
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions)
import Myo.Data.ProcError (ProcError)
import Myo.Effect.MState (MState, mmodify, mreads, mstate, mtrans)
import qualified Myo.Effect.Proc as Proc
import Myo.Effect.Proc (Proc)
import Myo.Interpreter.MState (interpretMState)

type ExecutionsState =
  MState (Map Ident Execution)

getExecution ::
  Member ExecutionsState r =>
  Ident ->
  Sem r (Maybe Execution)
getExecution =
  mreads . Map.lookup

withExecution ::
  Member ExecutionsState r =>
  Ident ->
  (Execution -> Sem r a) ->
  Sem r (Maybe a)
withExecution i f =
  traverse f =<< getExecution i

withExecution_ ::
  Member ExecutionsState r =>
  Ident ->
  (Execution -> Sem r a) ->
  Sem r ()
withExecution_ i f =
  void (withExecution i f)

modifyExecution ::
  Member ExecutionsState r =>
  Ident ->
  (Execution -> Sem r Execution) ->
  Sem r ()
modifyExecution ident f =
  mtrans (mapMOf (at ident . Lens._Just) f)

getState ::
  Member ExecutionsState r =>
  Ident ->
  Sem r (Maybe ExecutionState)
getState i =
  fmap state <$> getExecution i

running ::
  Member (Proc !! ProcError) r =>
  ExecutionState ->
  Sem r Bool
running = \case
  Tracked pid ->
    False <! Proc.exists pid
  Starting _ ->
    pure False
  Running ->
    pure True
  Pending ->
    pure False
  Unknown ->
    pure False
  Stopped ->
    pure False

active ::
  Member (Proc !! ProcError) r =>
  ExecutionState ->
  Sem r Bool
active = \case
  Tracked pid ->
    False <! Proc.exists pid
  Starting pid ->
    False <! Proc.exists pid
  Running ->
    pure True
  Pending ->
    pure True
  Unknown ->
    pure False
  Stopped ->
    pure False

checkState ::
  Member ExecutionsState r =>
  (ExecutionState -> Sem r Bool) ->
  Ident ->
  Sem r Bool
checkState f i =
  fmap (fromMaybe False) . traverse f =<< getState i

waitFor ::
  Members [ExecutionsState, Embed IO] r =>
  Ident ->
  Sem r ()
waitFor i =
  getExecution i >>= traverse_ \ e -> embed (readMVar (e ^. #sync . #wait))

interpretExecutions ::
  Members [Proc !! ProcError, ChronosTime, Log, Resource, Race, Mask mres, Embed IO] r =>
  InterpreterFor Executions r
interpretExecutions =
  interpretMState (mempty :: Map Ident Execution) .
  reinterpret \case
    Executions.Get i ->
      getExecution i
    Executions.Modify i f ->
      mstate \ s -> do
        case Map.lookup i s of
          Just e ->
            let (a, newE) = f e
            in (maybe (Map.delete i) (Map.insert i) newE s, Just a)
          Nothing ->
            (s, Nothing)
    Executions.Start i -> do
      Log.debug [exon|Starting execution `#{identText i}`|]
      -- archiveExecution ident
      now <- Time.now
      wait <- embed newEmptyMVar
      kill <- embed newEmptyMVar
      mmodify (Map.insert i (Execution i ExecutionState.Pending now (ExecutionSync wait kill)))
    Executions.Stop i -> do
      modifyExecution i \ e@Execution {sync = ExecutionSync {wait}} ->
        (e & #state .~ ExecutionState.Stopped) <$ embed (tryPutMVar wait ())
    Executions.Running i ->
      checkState running i
    Executions.Active i ->
      checkState active i
    Executions.Wait i ->
      withExecution_ i \ e -> embed (readMVar (e ^. #sync . #wait))
    Executions.Kill i ->
      withExecution_ i \ e -> embed (tryPutMVar (e ^. #sync . #kill) ())
    Executions.WaitKill i -> do
      withExecution_ i \ e ->
        embed (readMVar (e ^. #sync . #kill))
      withExecution_ i \ e ->
        for_ (Execution.pid e) (resume_ . Proc.kill)
