module Myo.Command.Add where

import Prelude hiding (lines)
import Ribosome (Handler)

import Myo.Command.Command (shellCommand, systemCommand)
import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions (..))
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions (..))
import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (..))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Data.Maybe (orFalse)

myoAddSystemCommand ::
  Member (AtomicState CommandState) r =>
  AddSystemCommandOptions ->
  Handler r ()
myoAddSystemCommand (AddSystemCommandOptions {..}) =
  atomicModify' (#commands %~ (cmd :))
  where
    cmd :: Command
    cmd =
      (systemCommand target ident lines) {
        Command.runner,
        lang,
        skipHistory = orFalse skipHistory,
        kill = orFalse kill,
        capture = orFalse capture,
        commandShell = orFalse commandShell
      }

myoAddShellCommand ::
  Member (AtomicState CommandState) r =>
  AddShellCommandOptions ->
  Handler r ()
myoAddShellCommand AddShellCommandOptions {..} =
  atomicModify' (#commands %~ (cmd :))
  where
    cmd :: Command
    cmd =
      (shellCommand target ident lines) {
        Command.runner,
        lang,
        skipHistory = orFalse skipHistory,
        kill = orFalse kill,
        capture = orFalse capture
      }
