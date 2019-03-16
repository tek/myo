module Myo.Command.Add(
  myoAddSystemCommand,
) where

import Control.Lens (Lens')
import qualified Ribosome.Control.Monad.Ribo as Ribo (prepend)
import Ribosome.Msgpack.NvimObject (NO(NO))

import Myo.Command.Command (systemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.Command (Command)
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (commands)
import Myo.Data.Myo (Myo)

myoAddSystemCommand :: NO AddSystemCommandOptions -> Myo ()
myoAddSystemCommand (NO (AddSystemCommandOptions ident lines' runner target lang)) =
  Ribo.prepend (CommandState.commands :: Lens' CommandState [Command]) cmd
  where
    cmd = systemCommand target ident lines' runner lang
