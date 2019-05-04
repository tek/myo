module Myo.Command.Run where

import Chiasma.Data.Ident (Ident, identText)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.DeepError (hoistEither, MonadDeepError)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Ribosome.Control.Monad.Ribo (MonadRibo)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Tmux.Run (RunTmux)

import Myo.Command.Command (commandByIdent)
import Myo.Command.Data.Command (Command(..))
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.Execution (ExecutionState)
import qualified Myo.Command.Data.Execution as ExecutionState (ExecutionState(Unknown))
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask(RunTask), RunTaskDetails)
import qualified Myo.Command.Data.RunTask as RunTask (RunTask(..))
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))
import Myo.Command.Execution (closeOutputSocket, isCommandActive, pushExecution)
import Myo.Command.History (lookupHistory, pushHistory)
import Myo.Command.Log (pushCommandLog)
import Myo.Command.Monitor (monitorCommand)
import Myo.Command.Runner (findRunner)
import Myo.Command.RunTask (runTask)
import Myo.Data.Env (Env, Runner(Runner, runnerCheckPending))
import Myo.Orphans ()
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Render (MyoRender)
import Myo.Ui.Toggle (ensurePaneOpen)

preRun ::
  RunTmux m =>
  MonadRibo m =>
  NvimE e m =>
  MonadDeepError e SettingError m =>
  MyoRender s e m =>
  MonadBaseControl IO m =>
  MonadDeepError e ToggleError m =>
  MonadDeepError e TreeModError m =>
  MonadDeepError e RunError m =>
  MonadDeepError e CommandError m =>
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  MonadThrow m =>
  RunTask ->
  Runner ->
  m ()
preRun task@(RunTask (Command _ cmdIdent _ _ _) log (RunTaskDetails.UiSystem ident)) runner = do
  ensurePaneOpen ident
  checkPending <- hoistEither =<< liftIO (runnerCheckPending runner task)
  closeOutputSocket cmdIdent
  pushExecution cmdIdent checkPending
  monitorCommand cmdIdent log
preRun (RunTask _ _ (RunTaskDetails.UiShell shellIdent _)) _ = do
  active <- isCommandActive shellIdent
  unless active $ do
    logDebug $ "starting inactive shell command `" <> identText shellIdent <> "`"
    myoRun shellIdent
preRun _ _ =
  return ()

postRun ::
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  RunTask ->
  m ()
postRun (RunTask cmd@(Command _ ident _ _ _) log (RunTaskDetails.UiSystem _)) =
  pushHistory cmd
postRun _ =
  return ()

executeRunner ::
  MonadRibo m =>
  MonadIO m =>
  MonadDeepError e RunError m =>
  MonadDeepState s Env m =>
  Runner ->
  RunTask ->
  m ()
executeRunner (Runner _ _ run _) task = do
  logDebug $ "executing runner for command `" <> identText ident <> "`"
  hoistEither =<< liftIO (run task)
  where
    ident = cmdIdent . RunTask.rtCommand $ task

runCommand ::
  RunTmux m =>
  MonadRibo m =>
  MyoRender s e m =>
  MonadBaseControl IO m =>
  MonadDeepError e ToggleError m =>
  MonadDeepError e TreeModError m =>
  MonadDeepError e RunError m =>
  MonadDeepError e CommandError m =>
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e SettingError m =>
  MonadThrow m =>
  Command ->
  m ()
runCommand cmd = do
  task <- runTask cmd
  runner <- findRunner task
  preRun task runner
  executeRunner runner task
  void $ postRun task

myoRun ::
  RunTmux m =>
  MonadRibo m =>
  MyoRender s e m =>
  MonadBaseControl IO m =>
  MonadDeepError e ToggleError m =>
  MonadDeepError e TreeModError m =>
  MonadDeepError e RunError m =>
  MonadDeepError e CommandError m =>
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e SettingError m =>
  MonadThrow m =>
  Ident ->
  m ()
myoRun =
  runCommand <=< commandByIdent

myoReRun ::
  RunTmux m =>
  MonadRibo m =>
  MyoRender s e m =>
  MonadBaseControl IO m =>
  MonadDeepError e ToggleError m =>
  MonadDeepError e TreeModError m =>
  MonadDeepError e RunError m =>
  MonadDeepError e CommandError m =>
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e SettingError m =>
  MonadThrow m =>
  Int ->
  m ()
myoReRun =
  runCommand <=< lookupHistory
