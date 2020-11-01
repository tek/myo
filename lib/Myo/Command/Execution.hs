module Myo.Command.Execution where

import Control.Lens (Lens')
import qualified Control.Lens as Lens (_Just, at, mapMOf, set, view)
import Network.Socket (Socket)
import qualified Network.Socket as Socket (close)
import Prelude hiding (state)
import Ribosome.Control.Exception (tryAny)
import qualified Ribosome.Control.Monad.Ribo as Ribo (prepend)
import System.Hourglass (timeCurrent)

import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (executing, executionLog)
import Myo.Command.Data.Execution (Execution(Execution), ExecutionMonitor(ExecutionMonitor), ExecutionState(..))
import qualified Myo.Command.Data.Execution as Execution (monitor)
import qualified Myo.Command.Data.Execution as ExecutionMonitor (socket, state)
import qualified Myo.Command.Data.Execution as ExecutionState (ExecutionState(..))
import Myo.Command.Data.Pid (Pid)
import Myo.System.Proc (processExists)

executionLens :: Ident -> Lens' CommandState (Maybe Execution)
executionLens ident = CommandState.executing . Lens.at ident

executions ::
  MonadDeepState s CommandState m =>
  m (Map Ident Execution)
executions =
  getL @CommandState CommandState.executing

archiveExecution ::
  MonadIO m =>
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  Ident ->
  m ()
archiveExecution ident =
  traverse_ archive =<< getL (executionLens ident)
  where
    archive execution = do
      logDebug $ "removing execution `" <> identText ident <> "`"
      Ribo.prepend @CommandState CommandState.executionLog (stopped execution)
    stopped =
      Lens.set (Execution.monitor . ExecutionMonitor.state) Stopped

removeExecution ::
  MonadIO m =>
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  Ident ->
  m ()
removeExecution ident = do
  archiveExecution ident
  setL @CommandState (executionLens ident) Nothing

killExecution ::
  MonadIO m =>
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  Ident ->
  m ()
killExecution ident = do
  logDebug $ "killing execution `" <> identText ident <> "`"
  closeOutputSocket ident
  removeExecution ident

pushExecution ::
  MonadIO m =>
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  Ident ->
  IO ExecutionState ->
  m ()
pushExecution ident checkPending = do
  logDebug $ "pushing execution `" <> identText ident <> "`"
  archiveExecution ident
  now <- liftIO timeCurrent
  setL @CommandState (executionLens ident) (Just (Execution ident "" [] (monitor now)))
  where
    monitor now =
      ExecutionMonitor ExecutionState.Pending now Nothing checkPending

findExecution ::
  MonadDeepState s CommandState m =>
  Ident ->
  m (Maybe Execution)
findExecution ident =
  gets $ Lens.view $ executionLens ident

executionRunning ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Execution ->
  m Bool
executionRunning (Execution _ _ _ (ExecutionMonitor state _ _ _)) =
  check state
  where
    check (Tracked pid) =
      processExists pid
    check (Starting _) =
      return False
    check Running =
      return True
    check Pending =
      return False
    check Unknown =
      return False
    check Stopped =
      return False

executionActive ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Execution ->
  m Bool
executionActive (Execution _ _ _ (ExecutionMonitor state _ _ _)) =
  check state
  where
    check (Tracked pid) =
      processExists pid
    check (Starting pid) =
      processExists pid
    check Running =
      return True
    check Pending =
      return True
    check Unknown =
      return False
    check Stopped =
      return False

isCommandRunning ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  Ident ->
  m Bool
isCommandRunning =
  maybe (return False) executionRunning <=< findExecution

isCommandActive ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  Ident ->
  m Bool
isCommandActive =
  maybe (return False) executionActive <=< findExecution

executionPid ::
  Execution ->
  Maybe Pid
executionPid (Execution _ _ _ (ExecutionMonitor (Tracked pid) _ _ _)) =
  Just pid
executionPid _ =
  Nothing

modifyExecution ::
  MonadDeepState s CommandState m =>
  (Execution -> m Execution) ->
  Ident ->
  m ()
modifyExecution f ident =
  modifyM $ Lens.mapMOf (executionLens ident . Lens._Just) f

modifyExecutionState ::
  MonadIO m =>
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  Ident ->
  (ExecutionState -> m ExecutionState) ->
  m ()
modifyExecutionState ident f =
  modifyExecution (Lens.mapMOf (Execution.monitor . ExecutionMonitor.state) update) ident
  where
    update previous = do
      new <- f previous
      when (previous /= new) (logDebug (message previous new))
      return new
    message previous new =
      "changing execution state of `" <> identText ident <> "` from " <> show previous <> " to " <> show new

setExecutionState ::
  MonadIO m =>
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  Ident ->
  ExecutionState ->
  m ()
setExecutionState ident =
  modifyExecutionState ident . const . return

storeOutputSocket ::
  MonadIO m =>
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  Socket ->
  Ident ->
  m ()
storeOutputSocket socket ident = do
  logDebug $ "storing output socket for `" <> identText ident <> "`"
  modifyExecution (Lens.mapMOf (Execution.monitor . ExecutionMonitor.socket) (const (return $ Just socket))) ident

closeOutputSocket ::
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  Ident ->
  m ()
closeOutputSocket ident = do
  logDebug $ "closing output socket for `" <> identText ident <> "`"
  modifyM (Lens.mapMOf (executionLens ident . Lens._Just . Execution.monitor . ExecutionMonitor.socket) close)
  where
    close socket =
      Nothing <$ liftIO (tryAny $ traverse_ Socket.close socket)
