module Myo.Command.Execution where

import Chiasma.Data.Ident (Ident, identText)
import qualified Control.Lens as Lens (_Just, at, mapMOf, set)
import Exon (exon)
import qualified Log
import Network.Socket (Socket)
import qualified Network.Socket as Socket (close)
import Polysemy.Chronos (ChronosTime)
import qualified Time

import qualified Myo.Command.Data.CommandState as CommandState
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.Execution (Execution (Execution), ExecutionMonitor (ExecutionMonitor))
import qualified Myo.Command.Data.ExecutionState as ExecutionState
import Myo.Command.Data.ExecutionState (ExecutionState (..))
import Process (Pid)
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions)

executionLens :: Ident -> Lens' CommandState (Maybe Execution)
executionLens ident =
  #executing . Lens.at ident

executions ::
  Member (AtomicState CommandState) r =>
  Sem r (Map Ident Execution)
executions =
  atomicGets CommandState.executing

archiveExecution ::
  Members [AtomicState CommandState, Log] r =>
  Ident ->
  Sem r ()
archiveExecution ident =
  traverse_ archive =<< atomicView (executionLens ident)
  where
    archive execution = do
      Log.debug [exon|removing execution `#{identText ident}`|]
      atomicModify' (#executionLog %~ (stopped execution :))
    stopped =
      Lens.set (#monitor . #state) Stopped

removeExecution ::
  Members [AtomicState CommandState, Log] r =>
  Ident ->
  Sem r ()
removeExecution ident = do
  archiveExecution ident
  atomicSet (executionLens ident) Nothing

killExecution ::
  Members [Executions, AtomicState CommandState, Log, Embed IO] r =>
  Ident ->
  Sem r ()
killExecution ident = do
  Log.debug [exon|killing execution `#{identText ident}`|]
  closeOutputSocket ident
  removeExecution ident

pushExecution ::
  Members [AtomicState CommandState, ChronosTime, Log] r =>
  Ident ->
  Sem r ()
pushExecution ident = do
  Log.debug $ "pushing execution `" <> identText ident <> "`"
  archiveExecution ident
  now <- Time.now
  atomicSet (executionLens ident) (Just (Execution ident "" [] (monitor now)))
  where
    monitor now =
      ExecutionMonitor ExecutionState.Pending now Nothing

executionPid ::
  Execution ->
  Maybe Pid
executionPid (Execution _ _ _ (ExecutionMonitor (Tracked pid) _ _)) =
  Just pid
executionPid _ =
  Nothing

modifyExecution ::
  Member (AtomicState CommandState) r =>
  (Execution -> Sem r Execution) ->
  Ident ->
  Sem r ()
modifyExecution f ident =
  atomicPut =<< Lens.mapMOf (executionLens ident . Lens._Just) f =<< atomicGet

modifyExecutionState ::
  Members [AtomicState CommandState, Log] r =>
  Ident ->
  (ExecutionState -> Sem r ExecutionState) ->
  Sem r ()
modifyExecutionState ident f =
  modifyExecution (Lens.mapMOf (#monitor . #state) update) ident
  where
    update previous = do
      new <- f previous
      when (previous /= new) (Log.debug (message previous new))
      pure new
    message previous new =
      "changing execution state of `" <> identText ident <> "` from " <> show previous <> " to " <> show new

setExecutionState ::
  Members [AtomicState CommandState, Log] r =>
  Ident ->
  ExecutionState ->
  Sem r ()
setExecutionState ident =
  modifyExecutionState ident . const . pure

storeOutputSocket ::
  Members [AtomicState CommandState, Log] r =>
  Socket ->
  Ident ->
  Sem r ()
storeOutputSocket socket ident = do
  Log.debug $ "storing output socket for `" <> identText ident <> "`"
  modifyExecution (Lens.mapMOf (#monitor . #socket) (const (pure $ Just socket))) ident

closeOutputSocket ::
  Members [Executions, AtomicState CommandState, Log, Embed IO] r =>
  Ident ->
  Sem r ()
closeOutputSocket ident = do
  Log.debug [exon|closing output socket for `#{identText ident}`|]
  traverse_ close =<< Executions.update ident (#monitor . #socket <<.~ Nothing)
  where
    close socket =
      tryAny_ (traverse_ Socket.close socket)
