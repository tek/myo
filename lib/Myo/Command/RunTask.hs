module Myo.Command.RunTask(
  runTask,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState)
import Chiasma.Data.Ident (Ident)
import Myo.Command.Data.Command (Command(..))
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask(..), RunTaskDetails)
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter (CommandInterpreter(..))
import Myo.Data.Env (Env)

uiSystemTaskDetails :: Ident -> RunTaskDetails
uiSystemTaskDetails = undefined

systemTaskDetails :: RunTaskDetails
systemTaskDetails = undefined

uiShellTaskDetails :: Ident -> Ident -> RunTaskDetails
uiShellTaskDetails = undefined

vimTaskDetails :: RunTaskDetails
vimTaskDetails = undefined

runDetails ::
  (MonadError RunError m, MonadState Env m) =>
  Command ->
  m RunTaskDetails
runDetails (Command ident interpreter) = return $ analyze interpreter
  where
    analyze (CommandInterpreter.System (Just paneIdent)) = uiSystemTaskDetails paneIdent
    analyze (CommandInterpreter.System Nothing) = systemTaskDetails
    analyze (CommandInterpreter.Shell paneIdent) = uiShellTaskDetails ident paneIdent
    analyze (CommandInterpreter.Vim _ _) = vimTaskDetails

commandLog ::
  (MonadError RunError m, MonadState Env m) =>
  Ident ->
  m FilePath
commandLog = undefined

runTask ::
  (MonadError RunError m, MonadState Env m) =>
  Command ->
  m RunTask
runTask cmd = do
  details <- runDetails cmd
  cmdLog <- commandLog (cmdIdent cmd)
  return $ RunTask cmd cmdLog details
