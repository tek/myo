module Myo.Command.RunTaskDetails(
  runDetails,
) where

import Chiasma.Data.Ident (Ident)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState)

import Myo.Command.Data.Command (Command(..))
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter (CommandInterpreter(..))
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTaskDetails)
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))
import Myo.Data.Env (Env)

systemTaskDetails :: RunTaskDetails
systemTaskDetails =
  RunTaskDetails.System

uiSystemTaskDetails :: Ident -> RunTaskDetails
uiSystemTaskDetails = RunTaskDetails.UiSystem

uiShellTaskDetails ::
  (MonadError RunError m, MonadState Env m) =>
  Ident ->
  Ident ->
  m RunTaskDetails
uiShellTaskDetails = do
  undefined

vimTaskDetails :: RunTaskDetails
vimTaskDetails = undefined

runDetails ::
  (MonadError RunError m, MonadState Env m) =>
  Command ->
  m RunTaskDetails
runDetails (Command interpreter ident _ _) = analyze interpreter
  where
    analyze (CommandInterpreter.System Nothing) = return systemTaskDetails
    analyze (CommandInterpreter.System (Just paneIdent)) = return $ uiSystemTaskDetails paneIdent
    analyze (CommandInterpreter.Shell paneIdent) = uiShellTaskDetails ident paneIdent
    analyze (CommandInterpreter.Vim _ _) = return vimTaskDetails
