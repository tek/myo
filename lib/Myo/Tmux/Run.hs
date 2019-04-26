module Myo.Tmux.Run where

import Chiasma.Codec.Data.PanePid (PanePid)
import qualified Chiasma.Codec.Data.PanePid as PanePid (PanePid(panePid))
import Chiasma.Command.Pane (panePid, sendKeys)
import Chiasma.Data.Ident (identText)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.Views (Views, ViewsError)
import qualified Chiasma.Monad.Stream as Chiasma (runTmux)
import Chiasma.Native.Api (TmuxNative(TmuxNative))
import qualified Chiasma.View.State as Views (paneId)
import Control.Monad.DeepError (MonadDeepError, throwHoist)
import Control.Monad.DeepState (MonadDeepState)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Path (Abs, File, Path)
import Path.IO (doesFileExist)
import Ribosome.Control.Concurrent.Wait (waitIOPredDef)
import Ribosome.Control.Monad.Ribo (MonadRibo)
import Ribosome.Tmux.Run (RunTmux, runRiboTmux)

import Myo.Command.Data.Command (Command(Command))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.Pid (Pid(Pid))
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(SocketFailure))
import Myo.Command.Data.RunTask (RunTask(RunTask))
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))
import Myo.Command.Log (pipePaneToSocket)
import Myo.Command.Monitor (monitorCommand)
import Myo.System.Proc (childPids)
import Ribosome.Config.Setting (settingMaybe)
import Ribosome.Config.Settings (tmuxSocket)

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
tmuxRun (RunTask (Command _ commandIdent lines' _ _) logPath details) =
  run details
  where
    run (RunTaskDetails.UiSystem paneIdent) = do
      logDebug $ "running tmux system task `" <> identText commandIdent <> "`"
      socket <- settingMaybe tmuxSocket
      paneId <- Views.paneId paneIdent
      monitorCommand commandIdent logPath (Just (findTmuxPid socket paneId))
      waitForSocket logPath
      runRiboTmux $ do
        pipePaneToSocket paneId logPath
        send paneId
    run (RunTaskDetails.UiShell _ paneIdent) = do
      paneId <- Views.paneId paneIdent
      runRiboTmux $ send paneId
    run _ =
      undefined
    send paneId =
      sendKeys paneId (toString <$> lines')
