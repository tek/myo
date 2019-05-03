module Myo.Command.Run where

import Chiasma.Data.Ident (Ident, identText)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.DeepError (MonadDeepError, hoistEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Ribosome.Control.Monad.Ribo (MonadRibo)
import Ribosome.Tmux.Run (RunTmux)

import Myo.Command.Command (commandByIdent)
import Myo.Command.Data.Command (Command(..))
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.Execution as ExecutionState (ExecutionState(Unknown))
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask, RunTaskDetails)
import qualified Myo.Command.Data.RunTask as RunTask (RunTask(..))
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))
import Myo.Command.Execution (isCommandRunning, pushExecution)
import Myo.Command.History (lookupHistory, pushHistory)
import Myo.Command.Log (pushCommandLog)
import Myo.Command.RunTask (runTask)
import Myo.Command.Runner (findRunner)
import Myo.Data.Env (Env, Runner(Runner))
import Myo.Orphans ()
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Render (MyoRender)
import Myo.Ui.Toggle (ensurePaneOpen)

ensurePrerequisites ::
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
  MonadThrow m =>
  RunTaskDetails ->
  m ()
ensurePrerequisites (RunTaskDetails.UiSystem ident) =
  ensurePaneOpen ident
ensurePrerequisites (RunTaskDetails.UiShell shellIdent _) = do
  running <- isCommandRunning shellIdent
  unless running (myoRun shellIdent)
ensurePrerequisites _ =
  return ()

executeRunner ::
  MonadRibo m =>
  MonadIO m =>
  MonadDeepError e RunError m =>
  MonadDeepState s Env m =>
  Runner ->
  RunTask ->
  m ()
executeRunner (Runner _ _ run) task = do
  logDebug $ "executing runner for command `" <> identText ident <> "`"
  r <- liftIO $ run task
  hoistEither r
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
  MonadThrow m =>
  Command ->
  m ()
runCommand cmd = do
  task <- runTask cmd
  ensurePrerequisites (RunTask.rtDetails task)
  runner <- findRunner task
  pushExecution ident (const (return ExecutionState.Unknown))
  executeRunner runner task
  pushHistory cmd
  where
    ident = cmdIdent cmd

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
  MonadThrow m =>
  Int ->
  m ()
myoReRun =
  runCommand <=< lookupHistory
