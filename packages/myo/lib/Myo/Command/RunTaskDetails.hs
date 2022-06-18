module Myo.Command.RunTaskDetails where

import Chiasma.Data.Ident (Ident)

import Myo.Command.Command (commandByIdent)
import Myo.Command.Data.Command (Command (..))
import qualified Myo.Command.Data.Command as Command (interpreter)
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter (CommandInterpreter (..))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError (..))
import Myo.Command.Data.RunTask (RunTaskDetails)
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails (..))

systemTaskDetails :: RunTaskDetails
systemTaskDetails =
  RunTaskDetails.System

uiSystemTaskDetails :: Ident -> RunTaskDetails
uiSystemTaskDetails =
  RunTaskDetails.UiSystem

uiShellTaskDetails ::
  Members [AtomicState CommandState, Stop RunError, Stop CommandError] r =>
  Ident ->
  Sem r RunTaskDetails
uiShellTaskDetails shellIdent =
  RunTaskDetails.UiShell shellIdent <$> (extractTarget =<< interpreter)
  where
    interpreter =
      Command.interpreter <$> commandByIdent "uiShellTaskDetails-interpreter" shellIdent
    extractTarget (CommandInterpreter.System (Just target)) =
      pure target
    extractTarget _ = do
      shell <- commandByIdent "uiShellTaskDetails-target" shellIdent
      stop (RunError.InvalidShell shell)

vimTaskDetails :: RunTaskDetails
vimTaskDetails =
  RunTaskDetails.Vim

runDetails ::
  Members [AtomicState CommandState, Stop RunError, Stop CommandError] r =>
  Command ->
  Sem r RunTaskDetails
runDetails =
  analyze . Command.interpreter
  where
    analyze (CommandInterpreter.System Nothing) = pure systemTaskDetails
    analyze (CommandInterpreter.System (Just paneIdent)) = pure $ uiSystemTaskDetails paneIdent
    analyze (CommandInterpreter.Shell shellIdent) = uiShellTaskDetails shellIdent
    analyze (CommandInterpreter.Vim _ _) = pure vimTaskDetails
