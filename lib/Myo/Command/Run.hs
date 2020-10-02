module Myo.Command.Run where

import Chiasma.Data.Ident (generateIdent)
import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Chiasma.Ui.Data.TreeModError (TreeModError)
import qualified Control.Lens as Lens (view)
import Control.Monad.Catch (MonadThrow)
import qualified Data.Text as Text
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)

import Myo.Command.Command (commandByIdent, commandByIdentOrName, mayCommandByIdent, shellCommand, systemCommand)
import Myo.Command.Data.Command (Command(..))
import qualified Myo.Command.Data.Command as Command (ident)
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(NoLinesSpecified))
import Myo.Command.Data.RunLineOptions (RunLineOptions(RunLineOptions))
import Myo.Command.Data.RunTask (RunTask(RunTask))
import qualified Myo.Command.Data.RunTask as RunTask (RunTask(..))
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))
import Myo.Command.Execution (closeOutputSocket, isCommandActive, pushExecution)
import Myo.Command.History (lookupHistory, pushHistory)
import Myo.Command.Log (pushCommandLog)
import Myo.Command.Monitor (monitorCommand)
import Myo.Command.RunTask (runTask)
import Myo.Command.Runner (findRunner)
import Myo.Data.Env (Env, Runner(Runner, runnerCheckPending))
import Myo.Data.Maybe (orFalse)
import Myo.Orphans ()
import Myo.Save (preCommandSave)
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Render (MyoRender)
import Myo.Ui.Toggle (ensurePaneOpen)

preRunSystem ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e RunError m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s CommandState m =>
  RunTask ->
  Runner ->
  m ()
preRunSystem task@(RunTask (Command _ cmdIdent _ _ _ _ _ _ _) log _) runner = do
  checkPending <- hoistEither =<< liftIO (runnerCheckPending runner task)
  closeOutputSocket cmdIdent
  pushExecution cmdIdent checkPending
  monitorCommand cmdIdent log

preRun ::
  MonadRibo m =>
  MyoRender s e m =>
  MonadBaseControl IO m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e PersistError m =>
  MonadDeepError e RunError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ToggleError m =>
  MonadDeepError e TreeModError m =>
  MonadDeepState s CommandState m =>
  MonadDeepState s Env m =>
  MonadThrow m =>
  RunTask ->
  Runner ->
  m ()
preRun task@(RunTask _ _ (RunTaskDetails.UiSystem ident)) runner = do
  ensurePaneOpen ident
  preRunSystem task runner
preRun (RunTask _ _ (RunTaskDetails.UiShell shellIdent _)) _ = do
  active <- isCommandActive shellIdent
  unless active $ do
    logDebug $ "starting inactive shell command `" <> identText shellIdent <> "`"
    myoRun (identText shellIdent)
preRun task@(RunTask _ _ RunTaskDetails.System) runner = do
  preRunSystem task runner
preRun _ _ =
  return ()

postRun ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadThrow m =>
  RunTask ->
  m ()
postRun (RunTask cmd _ (RunTaskDetails.UiSystem _)) =
  pushHistory cmd
postRun (RunTask cmd _ (RunTaskDetails.UiShell _ _)) =
  pushHistory cmd
postRun _ =
  return ()

executeRunner ::
  MonadRibo m =>
  MonadDeepError e RunError m =>
  Runner ->
  RunTask ->
  m ()
executeRunner (Runner _ _ run _ _) task = do
  logDebug $ "executing runner for command `" <> identText ident <> "`"
  hoistEither =<< liftIO (run task)
  where
    ident = Lens.view Command.ident . RunTask.rtCommand $ task

-- |Main entry point for running commands that ensures consistency.
-- Saves all buffers, updating the 'lastSave' timestamp.
-- Selects the proper runner for the task, e.g. tmux.
-- Sets up the output watcher threads that connect to a socket; the implementation of the runner is expected to ensure
-- that output is redirected to this socket.
-- Pushes the command into the history.
runCommand ::
  MonadRibo m =>
  MyoRender s e m =>
  MonadBaseControl IO m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e PersistError m =>
  MonadDeepError e RunError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ToggleError m =>
  MonadDeepError e TreeModError m =>
  MonadDeepState s CommandState m =>
  MonadDeepState s Env m =>
  MonadThrow m =>
  Command ->
  m ()
runCommand cmd = do
  preCommandSave
  pushCommandLog (Lens.view Command.ident cmd)
  task <- runTask cmd
  runner <- findRunner task
  preRun task runner
  executeRunner runner task
  void $ postRun task

myoRunIdent ::
  MonadRibo m =>
  MyoRender s e m =>
  MonadBaseControl IO m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e PersistError m =>
  MonadDeepError e RunError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ToggleError m =>
  MonadDeepError e TreeModError m =>
  MonadDeepState s CommandState m =>
  MonadDeepState s Env m =>
  MonadThrow m =>
  Ident ->
  m ()
myoRunIdent =
  runCommand <=< commandByIdent "run"

myoRun ::
  MonadRibo m =>
  MyoRender s e m =>
  MonadBaseControl IO m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e PersistError m =>
  MonadDeepError e RunError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ToggleError m =>
  MonadDeepError e TreeModError m =>
  MonadDeepState s CommandState m =>
  MonadDeepState s Env m =>
  MonadThrow m =>
  Text ->
  m ()
myoRun =
  runCommand <=< commandByIdentOrName "run" . Text.strip

myoReRun ::
  MonadRibo m =>
  MyoRender s e m =>
  MonadBaseControl IO m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e PersistError m =>
  MonadDeepError e RunError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ToggleError m =>
  MonadDeepError e TreeModError m =>
  MonadDeepState s CommandState m =>
  MonadDeepState s Env m =>
  MonadThrow m =>
  Either Ident Int ->
  m ()
myoReRun =
  runCommand <=< lookupHistory

defaultTarget :: Ident
defaultTarget =
  Ident.Str "make"

myoLine ::
  MonadRibo m =>
  MyoRender s e m =>
  MonadBaseControl IO m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e PersistError m =>
  MonadDeepError e RunError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ToggleError m =>
  MonadDeepError e TreeModError m =>
  MonadDeepState s CommandState m =>
  MonadDeepState s Env m =>
  MonadThrow m =>
  RunLineOptions ->
  m ()
myoLine (RunLineOptions mayLine mayLines mayTarget runner lang skipHistory kill capture) = do
  ident <- generateIdent
  lines' <- hoistMaybe RunError.NoLinesSpecified (mayLines <|> (pure <$> mayLine))
  target <- maybe (pure (Right defaultTarget)) findTarget mayTarget
  runCommand $ cmd ident target lines'
  where
    cmd ident target lines' =
      cons target ident lines' runner lang Nothing (orFalse skipHistory) (orFalse kill) (orFalse capture)
    cons =
      either shellCommand (systemCommand . Just)
    findTarget target =
      maybe (Right target) (Left . Lens.view Command.ident) <$> mayCommandByIdent target

myoLineCmd ::
  MyoRender s e m =>
  MonadBaseControl IO m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e PersistError m =>
  MonadDeepError e RunError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ToggleError m =>
  MonadDeepError e TreeModError m =>
  MonadDeepState s CommandState m =>
  MonadDeepState s Env m =>
  MonadThrow m =>
  Text ->
  m ()
myoLineCmd line' =
  myoLine (RunLineOptions (Just line') Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

myo ::
  MyoRender s e m =>
  MonadBaseControl IO m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e PersistError m =>
  MonadDeepError e RunError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ToggleError m =>
  MonadDeepError e TreeModError m =>
  MonadDeepState s CommandState m =>
  MonadDeepState s Env m =>
  MonadThrow m =>
  Text ->
  m ()
myo =
  myoLineCmd
