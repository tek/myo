module Myo.Command.Add where

import Prelude hiding (lines)
import Ribosome (Handler, resumeReport)

import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions (..))
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions (..))
import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (..), shellCommand, systemCommand)
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandSpec (CommandSpec (CommandSpec))
import Myo.Data.Maybe (orFalse)
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)

myoAddSystemCommand ::
  Member (Commands !! CommandError) r =>
  AddSystemCommandOptions ->
  Handler r ()
myoAddSystemCommand AddSystemCommandOptions {..} =
  resumeReport (Commands.add cmd)
  where
    cmd :: Command
    cmd =
      (systemCommand target ident (CommandSpec lines (fold params))) {
        Command.lang,
        skipHistory = orFalse skipHistory,
        kill = orFalse kill,
        capture = orFalse capture,
        maxLogBytes,
        commandShell = orFalse commandShell
      }

myoAddShellCommand ::
  Member (Commands !! CommandError) r =>
  AddShellCommandOptions ->
  Handler r ()
myoAddShellCommand AddShellCommandOptions {..} =
  resumeReport (Commands.add cmd)
  where
    cmd :: Command
    cmd =
      (shellCommand target ident (CommandSpec lines (fold params))) {
        Command.lang,
        skipHistory = orFalse skipHistory,
        kill = orFalse kill,
        capture = orFalse capture,
        maxLogBytes
      }
