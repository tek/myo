module Myo.Command.Update where

import Data.MessagePack (Object)
import Prelude hiding (lines)
import Ribosome (Handler, SettingError, Settings, fromMsgpack, mapReport)
import qualified Ribosome.Settings as Settings

import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions (..))
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions (..))
import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (..), shellCommand, systemCommand)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandSettingCodec (CommandSettingCodec (CommandSettingCodec))
import Myo.Command.Data.CommandSpec (CommandSpec (CommandSpec))
import Myo.Data.Maybe (orFalse)
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)
import qualified Myo.Settings as Settings

updateCommands ::
  Member Commands r =>
  CommandSettingCodec ->
  Sem r ()
updateCommands (CommandSettingCodec system shell) =
  traverse_ Commands.add commands
  where
    commands = (createShell <$> fold shell) ++ (createSystem <$> fold system)

    createSystem :: AddSystemCommandOptions -> Command
    createSystem AddSystemCommandOptions {..} =
      (systemCommand target ident (CommandSpec lines (fold params))) {
        Command.lang,
        displayName,
        skipHistory = orFalse skipHistory,
        kill = orFalse kill,
        capture = orFalse capture,
        maxLogBytes,
        commandShell = orFalse commandShell
      }
    createShell :: AddShellCommandOptions -> Command
    createShell AddShellCommandOptions {..} =
      (shellCommand target ident (CommandSpec lines (fold params))) {
        Command.lang,
        displayName,
        skipHistory = orFalse skipHistory,
        kill = orFalse kill,
        capture = orFalse capture,
        maxLogBytes
      }

fetchCommands ::
  Members [Commands, Settings !! SettingError] r =>
  Sem r ()
fetchCommands =
  traverse_ updateCommands =<< Settings.maybe Settings.commands

myoUpdateCommands ::
  Member (Commands !! CommandError) r =>
  Object ->
  Handler r ()
myoUpdateCommands o =
  mapReport do
    restop . updateCommands =<< stopEitherWith (CommandError.Misc . show) (fromMsgpack o)
