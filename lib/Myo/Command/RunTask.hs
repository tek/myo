module Myo.Command.RunTask(
  runTask,
) where

import Chiasma.Data.Ident (Ident, identString)
import Control.Lens (Lens', (?~))
import qualified Control.Lens as Lens (view, at)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState, gets, modify)
import System.FilePath ((</>))

import Myo.Command.Data.Command (Command(..))
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter (CommandInterpreter(..))
import Myo.Command.Data.CommandState (Logs)
import qualified Myo.Command.Data.CommandState as CommandState (_logs)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask(..), RunTaskDetails)
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))
import Myo.Data.Env (Env)
import qualified Myo.Data.Env as Env (_tempDir, _command)

uiSystemTaskDetails :: Ident -> RunTaskDetails
uiSystemTaskDetails = undefined

systemTaskDetails :: RunTaskDetails
systemTaskDetails =
  RunTaskDetails.System

uiShellTaskDetails :: Ident -> Ident -> RunTaskDetails
uiShellTaskDetails = undefined

vimTaskDetails :: RunTaskDetails
vimTaskDetails = undefined

runDetails ::
  (MonadError RunError m, MonadState Env m) =>
  Command ->
  m RunTaskDetails
runDetails (Command interpreter ident _ _) = return $ analyze interpreter
  where
    analyze (CommandInterpreter.System (Just paneIdent)) = uiSystemTaskDetails paneIdent
    analyze (CommandInterpreter.System Nothing) = systemTaskDetails
    analyze (CommandInterpreter.Shell paneIdent) = uiShellTaskDetails ident paneIdent
    analyze (CommandInterpreter.Vim _ _) = vimTaskDetails

logLens :: Ident -> Lens' Env (Maybe FilePath)
logLens ident = Env._command . CommandState._logs . Lens.at ident

logByIdent ::
  (MonadError RunError m, MonadState Env m) =>
  Ident ->
  m (Maybe FilePath)
logByIdent ident =
  gets $ Lens.view $ logLens ident

logTempDir ::
  (MonadState Env m) =>
  m FilePath
logTempDir =
  gets $ Lens.view Env._tempDir

insertLog ::
  (MonadError RunError m, MonadState Env m) =>
  Ident ->
  m FilePath
insertLog ident = do
  base <- logTempDir
  let logPath = base </> "pane-" ++ identString ident
  modify $ logLens ident ?~ logPath
  return logPath

commandLog ::
  (MonadError RunError m, MonadState Env m) =>
  Ident ->
  m FilePath
commandLog ident = do
  existing <- logByIdent ident
  maybe (insertLog ident) return existing

runTask ::
  (MonadError RunError m, MonadState Env m) =>
  Command ->
  m RunTask
runTask cmd = do
  details <- runDetails cmd
  cmdLog <- commandLog (cmdIdent cmd)
  return $ RunTask cmd cmdLog details
