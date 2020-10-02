module Myo.Command.Update where

import qualified Control.Lens as Lens (set)
import Data.MessagePack (Object)
import Ribosome.Msgpack.Decode (fromMsgpack')
import Ribosome.Msgpack.Error (DecodeError)

import Myo.Command.Command (shellCommand, systemCommand)
import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions(AddShellCommandOptions))
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.CommandSettingCodec (CommandSettingCodec(CommandSettingCodec))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (commands)
import Myo.Data.Maybe (orFalse)

updateCommands ::
  MonadDeepError e DecodeError m =>
  MonadDeepState s CommandState m =>
  Object ->
  m ()
updateCommands o = do
  CommandSettingCodec system shell <- fromMsgpack' o
  modify @CommandState (Lens.set CommandState.commands ((createShell <$> fold shell) ++ (createSystem <$> fold system)))
  where
    createSystem (AddSystemCommandOptions ident lines' runner target lang name skipHistory kill capture) =
      systemCommand target ident lines' runner lang name (orFalse skipHistory) (orFalse kill) (orFalse capture)
    createShell (AddShellCommandOptions ident lines' runner target lang name skipHistory kill capture) =
      shellCommand target ident lines' runner lang name (orFalse skipHistory) (orFalse kill) (orFalse capture)
