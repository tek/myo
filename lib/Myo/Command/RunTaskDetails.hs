module Myo.Command.RunTaskDetails where

import Chiasma.Data.Ident (Ident)
import qualified Control.Lens as Lens (view)
import Control.Monad.DeepError (MonadDeepError)

import Myo.Command.Command (commandByIdent)
import Myo.Command.Data.Command (Command(..))
import qualified Myo.Command.Data.Command as Command (interpreter)
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter (CommandInterpreter(..))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(..))
import Myo.Command.Data.RunTask (RunTaskDetails)
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))

systemTaskDetails :: RunTaskDetails
systemTaskDetails =
  RunTaskDetails.System

uiSystemTaskDetails :: Ident -> RunTaskDetails
uiSystemTaskDetails = RunTaskDetails.UiSystem

uiShellTaskDetails ::
  MonadDeepState s CommandState m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e RunError m =>
  Ident ->
  m RunTaskDetails
uiShellTaskDetails shellIdent =
  RunTaskDetails.UiShell shellIdent <$> (extractTarget =<< interpreter)
  where
    interpreter =
      Lens.view Command.interpreter <$> commandByIdent "uiShellTaskDetails-interpreter" shellIdent
    extractTarget (CommandInterpreter.System (Just target)) =
      return target
    extractTarget _ = do
      shell <- commandByIdent "uiShellTaskDetails-target" shellIdent
      throwHoist (RunError.InvalidShell shell)

vimTaskDetails :: RunTaskDetails
vimTaskDetails = undefined

runDetails ::
  MonadDeepState s CommandState m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e RunError m =>
  Command ->
  m RunTaskDetails
runDetails (Command interpreter _ _ _ _ _ _) = analyze interpreter
  where
    analyze (CommandInterpreter.System Nothing) = return systemTaskDetails
    analyze (CommandInterpreter.System (Just paneIdent)) = return $ uiSystemTaskDetails paneIdent
    analyze (CommandInterpreter.Shell shellIdent) = uiShellTaskDetails shellIdent
    analyze (CommandInterpreter.Vim _ _) = return vimTaskDetails
