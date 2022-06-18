module Myo.Command.Add where

import Control.Lens ((%~))
import Ribosome (Handler)

import Myo.Command.Command (shellCommand, systemCommand)
import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions (AddShellCommandOptions))
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions (AddSystemCommandOptions))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Data.Maybe (orFalse)

myoAddSystemCommand ::
  Member (AtomicState CommandState) r =>
  AddSystemCommandOptions ->
  Handler r ()
myoAddSystemCommand (AddSystemCommandOptions ident lines' runner target lang name skipHistory kill capture) =
  atomicModify' (#commands %~ (cmd :))
  where
    cmd = systemCommand target ident lines' runner lang name (orFalse skipHistory) (orFalse kill) (orFalse capture)

myoAddShellCommand ::
  Member (AtomicState CommandState) r =>
  AddShellCommandOptions ->
  Handler r ()
myoAddShellCommand (AddShellCommandOptions ident lines' runner target lang name skipHistory kill capture) =
  atomicModify' (#commands %~ (cmd :))
  where
    cmd = shellCommand target ident lines' runner lang name (orFalse skipHistory) (orFalse kill) (orFalse capture)
