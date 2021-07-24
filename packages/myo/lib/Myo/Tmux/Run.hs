module Myo.Tmux.Run where

import qualified Chiasma.Codec.Data.PanePid as PanePid (PanePid(panePid))
import Chiasma.Command.Pane (capturePane, panePid, pipePane, quitCopyMode, sendKeys, paneTarget)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.Views (Views, ViewsError)
import Chiasma.Monad.Stream (TmuxProg)
import qualified Chiasma.Monad.Stream as Chiasma (runTmux)
import Chiasma.Native.Api (TmuxNative(TmuxNative))
import qualified Chiasma.View.State as Views (paneId)
import qualified Data.Text as Text
import qualified Myo.Control.Concurrent.Wait as Ribosome (waitIOPredDef)
import Path (Abs, File, Path)
import Path.IO (doesFileExist, removeFile)
import Prelude hiding (state)
import Ribosome.Api.Echo (echo, echon)
import Ribosome.Control.Exception (tryAny)
import Ribosome.Tmux.Run (RunTmux, runRiboTmux)
import qualified System.Posix.Signals as Signal
import System.Posix.Signals (Signal)

import Myo.Command.Data.Command (Command(Command))
import Myo.Command.Data.Execution (ExecutionState)
import qualified Myo.Command.Data.Execution as ExecutionState (ExecutionState(Starting, Pending, Unknown))
import Myo.Command.Data.Pid (Pid(Pid))
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask(RunTask), RunTaskDetails)
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))
import Myo.Command.Kill (signalPid)
import Myo.Command.Log (pipePaneToSocket)
import Myo.Control.Concurrent.Wait (waitIOPredDef)
import Myo.System.Proc (childPids)
import Ribosome.Config.Setting (settingMaybe)
import qualified Ribosome.Config.Settings as Settings (tmuxSocket)
import Control.Monad.Free (MonadFree)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import qualified Chiasma.Monad.Tmux as Tmux
import Data.List (dropWhileEnd)

tmuxCanRun :: RunTask -> Bool
tmuxCanRun (RunTask _ _ details) =
  checkCmd details
  where
    checkCmd (RunTaskDetails.UiSystem _) = True
    checkCmd (RunTaskDetails.UiShell _ _) = True
    checkCmd _ = False

waitForSocket ::
  MonadIO m =>
  MonadDeepError e RunError m =>
  Path Abs File ->
  m ()
waitForSocket logPath =
  Ribosome.waitIOPredDef (pure logPath) doesFileExist >>= \case
    Right _ -> return ()
    Left _ -> throwHoist RunError.SocketFailure

firstChildPid :: Pid -> IO (Maybe Pid)
firstChildPid pid =
  listToMaybe <$> childPids pid

findTmuxPid ::
  MonadIO m =>
  PaneId ->
  TmuxProg m (Maybe Pid)
findTmuxPid paneId =
  commandPid =<< extractShellPid <$> panePid paneId
  where
    extractShellPid =
      fmap (Pid . PanePid.panePid)
    commandPid pid =
      liftIO $ join <$> traverse firstChildPid pid

findTmuxPidIO ::
  Maybe FilePath ->
  PaneId ->
  IO (Maybe Pid)
findTmuxPidIO socket paneId =
  fromRight Nothing <$> runExceptT @TmuxError (Chiasma.runTmux (TmuxNative socket) (findTmuxPid paneId))

tmuxCheckPending ::
  NvimE e m =>
  RunTmux m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Views m =>
  MonadDeepError e ViewsError m =>
  RunTask ->
  m (IO ExecutionState)
tmuxCheckPending (RunTask _ logPath (RunTaskDetails.UiSystem paneIdent)) = do
  tmuxSocket <- settingMaybe Settings.tmuxSocket
  paneId <- Views.paneId paneIdent
  runRiboTmux $ pipePane paneId ""
  void $ tryAny $ removeFile logPath
  return $ state <$> findTmuxPidIO tmuxSocket paneId
  where
    state (Just pid) =
      ExecutionState.Starting pid
    state _ =
      ExecutionState.Pending
tmuxCheckPending _ =
  return (return ExecutionState.Unknown)

killSignals :: [Signal]
killSignals =
  [Signal.sigINT, Signal.sigTERM, Signal.sigKILL]

killRunning ::
  NvimE e m =>
  MonadRibo m =>
  PaneId ->
  TmuxProg m ()
killRunning paneId =
  killIfRunning =<< findTmuxPid paneId
  where
    killIfRunning =
      traverse_ kill
    kill pid = do
      echon "trying to kill the running process..."
      ifM (attemptKill killSignals (Just pid)) (echo "success") (echo "failed")
    attemptKill _ Nothing =
      pure True
    attemptKill [] _ =
      pure False
    attemptKill (sig : remainingSigs) (Just pid) = do
      liftIO (signalPid sig pid)
      awaitTermination >>= \case
        Right _ ->
          trypNextSignal remainingSigs
        Left _ ->
          pure True
    awaitTermination =
      waitIOPredDef (panePid paneId) (pure . isNothing)
    trypNextSignal sigs =
      attemptKill sigs =<< findTmuxPid paneId

tmuxRun ::
  RunTmux m =>
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Views m =>
  MonadDeepError e ViewsError m =>
  MonadDeepError e RunError m =>
  RunTask ->
  m ()
tmuxRun (RunTask (Command _ commandIdent lines' _ _ _ _ kill _) logPath details) =
  run details
  where
    run (RunTaskDetails.UiSystem paneIdent) = do
      logDebug $ "running tmux system task `" <> identText commandIdent <> "` with log `" <> show logPath <> "`"
      paneId <- Views.paneId paneIdent
      waitForSocket logPath
      runRiboTmux $ do
        when kill (killRunning paneId)
        pipePaneToSocket paneId logPath
        runUi paneId
    run (RunTaskDetails.UiShell _ paneIdent) = do
      logDebug $ "running tmux shell task `" <> identText commandIdent <> "`"
      paneId <- Views.paneId paneIdent
      runRiboTmux $ runUi paneId
    run _ =
      unit
    runUi paneId = do
        quitCopyMode paneId
        send paneId
    send paneId =
      sendKeys paneId lines'

taskPane :: RunTaskDetails -> Maybe Ident
taskPane = \case
  RunTaskDetails.UiSystem paneIdent ->
    Just paneIdent
  RunTaskDetails.UiShell _ paneIdent ->
    Just paneIdent
  _ ->
    Nothing

capturePaneClean ::
  MonadFree TmuxThunk m =>
  PaneId ->
  m [Text]
capturePaneClean paneId = do
  lines' <- Tmux.readRaw "capture-pane" (paneTarget paneId <> ["-p", "-J"])
  return $ dropWhileEnd ("" ==) lines'

tmuxCapture ::
  RunTmux m =>
  NvimE e m =>
  MonadDeepState s Views m =>
  MonadDeepError e RunError m =>
  MonadDeepError e ViewsError m =>
  RunTask ->
  m ByteString
tmuxCapture (RunTask _ _ details) = do
  paneIdent <- hoistMaybe (RunError.Unsupported "tmux" "capture") (taskPane details)
  paneId <- Views.paneId paneIdent
  foldMap (encodeUtf8 . flip Text.snoc '\n') <$> runRiboTmux (capturePaneClean paneId)
