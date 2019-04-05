module Myo.Command.Add where

import Control.Monad.DeepState (MonadDeepState)
import qualified Ribosome.Control.Monad.Ribo as Ribo (prepend)

import Myo.Command.Command (shellCommand, systemCommand)
import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions(AddShellCommandOptions))
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (commands)

myoAddSystemCommand ::
  MonadDeepState s CommandState m =>
  AddSystemCommandOptions ->
  m ()
myoAddSystemCommand (AddSystemCommandOptions ident lines' runner target lang) =
  Ribo.prepend @CommandState CommandState.commands cmd
  where
    cmd = systemCommand target ident lines' runner lang

myoAddShellCommand ::
  MonadDeepState s CommandState m =>
  AddShellCommandOptions ->
  m ()
myoAddShellCommand (AddShellCommandOptions ident lines' runner target lang) =
  Ribo.prepend @CommandState CommandState.commands cmd
  where
    cmd = shellCommand target ident lines' runner lang
