module Myo.Command.Update where

import Data.MessagePack (Object)
import Prelude hiding (lines)
import Ribosome (Handler, fromMsgpack, mapHandlerError)

import Myo.Command.Command (shellCommand, systemCommand)
import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions (..))
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions (..))
import Myo.Command.Data.Command (Command (..))
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandSettingCodec (CommandSettingCodec (CommandSettingCodec))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Data.Maybe (orFalse)

updateCommands ::
  Member (AtomicState CommandState) r =>
  Object ->
  Handler r ()
updateCommands o =
  mapHandlerError do
    CommandSettingCodec system shell <- stopEitherWith CommandError.Misc (fromMsgpack o)
    atomicModify' (#commands .~ ((createShell <$> fold shell) ++ (createSystem <$> fold system)))
  where
    createSystem :: AddSystemCommandOptions -> Command
    createSystem AddSystemCommandOptions {..} =
      (systemCommand target ident lines) {
        runner,
        lang,
        displayName,
        skipHistory = orFalse skipHistory,
        kill = orFalse kill,
        capture = orFalse capture
      }
    createShell :: AddShellCommandOptions -> Command
    createShell AddShellCommandOptions {..} =
      (shellCommand target ident lines) {
        runner,
        lang,
        displayName,
        skipHistory = orFalse skipHistory,
        kill = orFalse kill,
        capture = orFalse capture
      }
