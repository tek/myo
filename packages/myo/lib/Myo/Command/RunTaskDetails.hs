module Myo.Command.RunTaskDetails where

import Myo.Command.Data.Command (Command (..))
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter (CommandInterpreter (..))
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError (..))
import Myo.Command.Data.RunTask (RunTaskDetails)
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails (..))
import Myo.Command.Data.UiTarget (UiTarget)
import Myo.Data.CommandId (CommandId)
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)

systemTaskDetails :: RunTaskDetails
systemTaskDetails =
  RunTaskDetails.System

uiSystemTaskDetails :: UiTarget -> RunTaskDetails
uiSystemTaskDetails =
  RunTaskDetails.UiSystem

uiShellTaskDetails ::
  Members [Commands, Stop RunError] r =>
  CommandId ->
  Sem r RunTaskDetails
uiShellTaskDetails shellIdent =
  RunTaskDetails.UiShell shellIdent <$> (extractTarget =<< interpreter)
  where
    interpreter =
      (.interpreter) <$> Commands.queryId shellIdent
    extractTarget (CommandInterpreter.System (Just target)) =
      pure target
    extractTarget _ = do
      shell <- Commands.queryId shellIdent
      stop (RunError.InvalidShell shell)

runDetails ::
  Members [Commands, Stop RunError] r =>
  Command ->
  Sem r RunTaskDetails
runDetails =
  analyze . (.interpreter)
  where
    analyze (CommandInterpreter.System Nothing) = pure systemTaskDetails
    analyze (CommandInterpreter.System (Just paneIdent)) = pure $ uiSystemTaskDetails paneIdent
    analyze (CommandInterpreter.Shell shellIdent) = uiShellTaskDetails shellIdent
    analyze (CommandInterpreter.Vim silent target) = pure (RunTaskDetails.Vim silent target)
