module Myo.Command.Update where

import qualified Control.Lens as Lens (set)
import Data.MessagePack (Object)
import Ribosome.Msgpack.Decode (fromMsgpack')
import Ribosome.Msgpack.Error (DecodeError)

import Myo.Command.Command (shellCommand, systemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (commands)

updateSystemCommands ::
  MonadDeepError e DecodeError m =>
  MonadDeepState s CommandState m =>
  Object ->
  m ()
updateSystemCommands o = do
  cmdData <- fromMsgpack' o
  modify @CommandState (Lens.set CommandState.commands (create <$> cmdData))
  where
    create (AddSystemCommandOptions ident lines' runner target lang) =
      systemCommand target ident lines' runner lang
