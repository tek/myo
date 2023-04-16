module Myo.Command.Update where

import Data.MessagePack (Object)
import Prelude hiding (lines)
import Ribosome (Handler, SettingError, Settings, fromMsgpack, mapReport)
import qualified Ribosome.Settings as Settings

import Myo.Command.Command (shellCommand, systemCommand)
import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions (..))
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions (..))
import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (..))
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandSettingCodec (CommandSettingCodec (CommandSettingCodec))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Data.Maybe (orFalse)
import qualified Myo.Settings as Settings

updateCommands ::
  Member (AtomicState CommandState) r =>
  CommandSettingCodec ->
  Sem r ()
updateCommands (CommandSettingCodec system shell) =
  atomicModify' (#commands .~ ((createShell <$> fold shell) ++ (createSystem <$> fold system)))
  where
    createSystem :: AddSystemCommandOptions -> Command
    createSystem AddSystemCommandOptions {..} =
      (systemCommand target ident lines) {
        Command.runner,
        lang,
        displayName,
        skipHistory = orFalse skipHistory,
        kill = orFalse kill,
        capture = orFalse capture,
        commandShell = orFalse commandShell
      }
    createShell :: AddShellCommandOptions -> Command
    createShell AddShellCommandOptions {..} =
      (shellCommand target ident lines) {
        Command.runner,
        lang,
        displayName,
        skipHistory = orFalse skipHistory,
        kill = orFalse kill,
        capture = orFalse capture
      }

fetchCommands ::
  Members [AtomicState CommandState, Settings !! SettingError] r =>
  Sem r ()
fetchCommands =
  traverse_ updateCommands =<< Settings.maybe Settings.commands

myoUpdateCommands ::
  Member (AtomicState CommandState) r =>
  Object ->
  Handler r ()
myoUpdateCommands o =
  mapReport do
    updateCommands =<< stopEitherWith (CommandError.Misc . show) (fromMsgpack o)
