module Myo.Command.Interpreter.Executions where

import Chiasma.Data.Ident (Ident, identText)
import Conc (interpretAtomic)
import qualified Data.Map.Strict as Map
import Exon (exon)
import qualified Log
import Polysemy.Chronos (ChronosTime)
import qualified Time

import Myo.Command.Data.Execution (Execution (Execution), ExecutionMonitor (ExecutionMonitor))
import qualified Myo.Command.Data.ExecutionState as ExecutionState
import Myo.Command.Data.ExecutionState (ExecutionState (..))
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions)
import Myo.Data.ProcError (ProcError)
import qualified Myo.Effect.Proc as Proc
import Myo.Effect.Proc (Proc)

getExecution ::
  Member (AtomicState (Map Ident Execution)) r =>
  Ident ->
  Sem r (Maybe Execution)
getExecution =
  atomicGets . Map.lookup

getState ::
  Member (AtomicState (Map Ident Execution)) r =>
  Ident ->
  Sem r (Maybe ExecutionState)
getState i =
  fmap (view (#monitor . #state)) <$> getExecution i

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
  Member (AtomicState (Map Ident Execution)) r =>
  (ExecutionState -> Sem r Bool) ->
  Ident ->
  Sem r Bool
checkState f i =
  fmap (fromMaybe False) . traverse f =<< getState i

interpretExecutions ::
  Members [Proc !! ProcError, ChronosTime, Log, Embed IO] r =>
  InterpreterFor Executions r
interpretExecutions =
  interpretAtomic (mempty :: Map Ident Execution) .
  reinterpret \case
    Executions.Get i ->
      getExecution i
    Executions.Modify i f ->
      atomicState' \ s -> do
        case Map.lookup i s of
          Just e ->
            let (a, newE) = f e
            in (maybe (Map.delete i) (Map.insert i) newE s, Just a)
          Nothing ->
            (s, Nothing)
    Executions.Add i -> do
      Log.debug [exon|Adding execution `#{identText i}`|]
      -- archiveExecution ident
      now <- Time.now
      atomicModify' (Map.insert i (Execution i "" [] (ExecutionMonitor ExecutionState.Pending now Nothing)))
    Executions.Running i ->
      checkState running i
    Executions.Active i ->
      checkState active i
