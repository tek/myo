module Myo.Command.Update where

import Control.Lens ((.~))
import Data.MessagePack (Object)
import Ribosome (fromMsgpack)

import Myo.Command.Command (shellCommand, systemCommand)
import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions (AddShellCommandOptions))
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions (AddSystemCommandOptions))
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandSettingCodec (CommandSettingCodec (CommandSettingCodec))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Data.Maybe (orFalse)

updateCommands ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Object ->
  Sem r ()
updateCommands o = do
  CommandSettingCodec system shell <- stopEitherWith CommandError.Misc (fromMsgpack o)
  atomicModify' (#commands .~ ((createShell <$> fold shell) ++ (createSystem <$> fold system)))
  where
    createSystem (AddSystemCommandOptions ident lines' runner target lang name skipHistory kill capture) =
      systemCommand target ident lines' runner lang name (orFalse skipHistory) (orFalse kill) (orFalse capture)
    createShell (AddShellCommandOptions ident lines' runner target lang name skipHistory kill capture) =
      shellCommand target ident lines' runner lang name (orFalse skipHistory) (orFalse kill) (orFalse capture)
