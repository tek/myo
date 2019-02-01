module Myo.Command.Add(
  myoAddSystemCommand,
) where

import Ribosome.Msgpack.NvimObject (NO(NO))
import qualified Ribosome.Control.Ribo as Ribo (prepend)
import Myo.Command.Command (systemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import qualified Myo.Command.Data.CommandState as CommandState (_commands)
import qualified Myo.Data.Env as Env (_command)
import Myo.Data.Myo (Myo)

myoAddSystemCommand :: NO AddSystemCommandOptions -> Myo ()
myoAddSystemCommand (NO (AddSystemCommandOptions ident lines' runner target)) =
  Ribo.prepend (Env._command . CommandState._commands) cmd
  where
    cmd = systemCommand target ident lines' runner
