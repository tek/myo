module Myo.Command.Execution where

import Chiasma.Data.Ident (Ident)
import Control.Lens (Lens')
import qualified Control.Lens as Lens (_Just, at, over, set, view)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Ribosome.Control.Monad.Ribo as Ribo (prepend)
import System.Hourglass (timeCurrent)

import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (executing, executionLog)
import Myo.Command.Data.Execution (Execution(Execution), ExecutionMonitor(ExecutionMonitor), ExecutionState(..))
import qualified Myo.Command.Data.Execution as Execution (monitor)
import qualified Myo.Command.Data.Execution as ExecutionMonitor (state)
import qualified Myo.Command.Data.Execution as ExecutionState (ExecutionState(..))
import Myo.Command.Data.Pid (Pid)
import Myo.System.Proc (processExists)

executionLens :: Ident -> Lens' CommandState (Maybe Execution)
executionLens ident = CommandState.executing . Lens.at ident

archiveExecution ::
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  Ident ->
  m ()
archiveExecution ident =
  traverse_ archive =<< getL (executionLens ident)
  where
    archive execution = do
      logDebug @Text $ "removing execution `" <> identText ident <> "`"
      Ribo.prepend @CommandState CommandState.executionLog (stopped execution)
    stopped =
      Lens.set (Execution.monitor . ExecutionMonitor.state) Stopped

removeExecution ::
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  Ident ->
  m ()
removeExecution ident = do
  archiveExecution ident
  setL @CommandState (executionLens ident) Nothing

pushExecution ::
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  Ident ->
  (Ident -> IO ExecutionState) ->
  m ()
pushExecution ident checkPending = do
  logDebug @Text $ "pushing execution `" <> identText ident <> "`"
  archiveExecution ident
  now <- liftIO timeCurrent
  setL @CommandState (executionLens ident) (Just (Execution ident "" [] (monitor now)))
  where
    monitor now =
      ExecutionMonitor ExecutionState.Pending now checkPending

findExecution ::
  MonadDeepState s CommandState m =>
  Ident ->
  m (Maybe Execution)
findExecution ident =
  gets $ Lens.view $ executionLens ident

setExecutionRunning ::
  MonadRibo m =>
  Ident ->
  m ()
setExecutionRunning ident = do
  logDebug @Text $ "marking execution `" <> identText ident <> "` as running"
  undefined

executionRunning ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Execution ->
  m Bool
executionRunning (Execution _ _ _ (ExecutionMonitor (Tracked pid) _ _)) =
  processExists pid
executionRunning (Execution _ _ _ (ExecutionMonitor Running _ _)) =
  return True
executionRunning _ =
  return False

isCommandRunning ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  Ident ->
  m Bool
isCommandRunning =
  maybe (return False) executionRunning <=< findExecution

executionPid ::
  Execution ->
  Maybe Pid
executionPid (Execution _ _ _ (ExecutionMonitor (Tracked pid) _ _)) =
  Just pid
executionPid _ =
  Nothing

modifyExecutionState ::
  MonadDeepState s CommandState m =>
  (ExecutionState -> ExecutionState) ->
  Ident ->
  m ()
modifyExecutionState f ident =
  modifyL (executionLens ident) (Lens.over (Lens._Just . Execution.monitor . ExecutionMonitor.state) f)

setExecutionState ::
  MonadDeepState s CommandState m =>
  ExecutionState ->
  Ident ->
  m ()
setExecutionState =
  modifyExecutionState . const
