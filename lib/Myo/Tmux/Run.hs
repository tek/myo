module Myo.Tmux.Run where

import Chiasma.Codec.Data.PanePid (PanePid)
import qualified Chiasma.Codec.Data.PanePid as PanePid (PanePid(panePid))
import Chiasma.Command.Pane (panePid, pipePane, quitCopyMode, sendKeys)
import Chiasma.Data.Ident (identText)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.Views (Views, ViewsError)
import qualified Chiasma.Monad.Stream as Chiasma (runTmux)
import Chiasma.Native.Api (TmuxNative(TmuxNative))
import qualified Chiasma.View.State as Views (paneId)
import Control.Monad.Trans.Control (MonadBaseControl)
import Path (Abs, File, Path)
import Path.IO (doesFileExist, removeFile)
import Prelude hiding (state)
import Ribosome.Control.Concurrent.Wait (waitIOPredDef)
import Ribosome.Control.Exception (tryAny)
import Ribosome.Control.Monad.Ribo (MonadRibo)
import Ribosome.Tmux.Run (RunTmux, runRiboTmux)

import Myo.Command.Data.Command (Command(Command))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.Execution (ExecutionState)
import qualified Myo.Command.Data.Execution as ExecutionState (ExecutionState(Starting, Pending, Unknown))
import Myo.Command.Data.Pid (Pid(Pid))
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(SocketFailure))
import Myo.Command.Data.RunTask (RunTask(RunTask))
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))
import Myo.Command.Log (pipePaneToSocket)
import Myo.System.Proc (childPids)
import Ribosome.Config.Setting (settingMaybe)
import qualified Ribosome.Config.Settings as Settings (tmuxSocket)

tmuxCanRun :: RunTask -> Bool
tmuxCanRun (RunTask _ _ details) =
  checkCmd details
  where
    checkCmd (RunTaskDetails.UiSystem _) = True
    checkCmd (RunTaskDetails.UiShell _ _) = True
    checkCmd _ = False

waitForSocket ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e RunError m =>
  Path Abs File ->
  m ()
waitForSocket logPath =
  waitIOPredDef (pure logPath) doesFileExist >>= \case
    Right _ -> return ()
    Left _ -> throwHoist RunError.SocketFailure

firstChildPid :: Pid -> IO (Maybe Pid)
firstChildPid pid =
  listToMaybe <$> childPids pid

findTmuxPid ::
  Maybe FilePath ->
  PaneId ->
  IO (Maybe Pid)
findTmuxPid socket paneId =
  commandPid =<< extractShellPid <$> query
  where
    query :: IO (Either TmuxError (Maybe PanePid))
    query =
      runExceptT $ Chiasma.runTmux (TmuxNative socket) (panePid paneId)
    extractShellPid =
      fmap (Pid . PanePid.panePid) . fromRight Nothing
    commandPid =
      join <$$> traverse firstChildPid

tmuxCheckPending ::
  MonadBaseControl IO m =>
  MonadRibo m =>
  NvimE e m =>
  MonadIO m =>
  RunTmux m =>
  MonadDeepState s Views m =>
  MonadDeepError e ViewsError m =>
  RunTask ->
  m (IO ExecutionState)
tmuxCheckPending (RunTask _ logPath (RunTaskDetails.UiSystem paneIdent)) = do
  tmuxSocket <- settingMaybe Settings.tmuxSocket
  paneId <- Views.paneId paneIdent
  runRiboTmux $ pipePane paneId ""
  void $ tryAny $ removeFile logPath
  return $ state <$> findTmuxPid tmuxSocket paneId
  where
    state (Just pid) =
      ExecutionState.Starting pid
    state _ =
      ExecutionState.Pending
tmuxCheckPending _ =
  return (return ExecutionState.Unknown)

tmuxRun ::
  MonadRibo m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  RunTmux m =>
  MonadDeepState s CommandState m =>
  MonadDeepState s Views m =>
  MonadDeepError e ViewsError m =>
  MonadDeepError e RunError m =>
  RunTask ->
  m ()
tmuxRun (RunTask (Command _ commandIdent lines' _ _ _ _) logPath details) =
  run details
  where
    run (RunTaskDetails.UiSystem paneIdent) = do
      logDebug $ "running tmux system task `" <> identText commandIdent <> "`"
      paneId <- Views.paneId paneIdent
      waitForSocket logPath
      runRiboTmux $ do
        pipePaneToSocket paneId logPath
        quitCopyMode paneId
        send paneId
    run (RunTaskDetails.UiShell _ paneIdent) = do
      logDebug $ "running tmux shell task `" <> identText commandIdent <> "`"
      paneId <- Views.paneId paneIdent
      runRiboTmux $ send paneId
    run _ =
      undefined
    send paneId =
      sendKeys paneId lines'
