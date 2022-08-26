module Myo.Command.CommandMenu where

import qualified Chiasma.Data.Ident as Ident
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Exon (exon)
import Ribosome (
  Handler,
  Rpc,
  RpcError,
  ScratchId (ScratchId),
  Settings,
  mapReports,
  resumeReports,
  scratch,
  )
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Menu (
  MenuItem,
  MenuLoops,
  MenuResult (Error, Success),
  NvimMenuUi,
  WindowMenu,
  simpleMenuItem,
  staticNvimMenu,
  withFocus,
  )

import Myo.Command.Data.Command (Command (Command, cmdLines, displayName), ident)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import Myo.Data.CommandId (CommandId (CommandId))
import qualified Myo.Effect.Controller as Controller
import Myo.Effect.Controller (Controller)

menuItemName :: CommandId -> Maybe Text -> Text
menuItemName (CommandId ident) displayName =
  [exon|[#{fromMaybe (identTextShort ident) displayName}]|]
  where
    identTextShort = \case
      Ident.Str a ->
        a
      Ident.Uuid a ->
        Text.take 6 (UUID.toText a)

commandItems ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Sem r [MenuItem CommandId]
commandItems = do
  cmds <- stopNote CommandError.NoCommands . nonEmpty =<< atomicView #commands
  pure (toList (menuItem <$> cmds))
  where
    menuItem Command {..} =
      simpleMenuItem ident (menuItemText ident cmdLines displayName)
    menuItemText ident ls displayName =
      Text.unwords [menuItemName ident displayName, Text.take 100 (fromMaybe "<no command line>" (head ls))]

type CommandMenuStack ui =
  [
    NvimMenuUi ui,
    MenuLoops CommandId,
    Settings !! SettingError,
    Rpc !! RpcError,
    Log
  ]

commandMenu ::
  Members (CommandMenuStack ui) r =>
  Members [AtomicState CommandState, Stop CommandError, Stop RpcError] r =>
  Sem r (MenuResult CommandId)
commandMenu = do
  items <- commandItems
  staticNvimMenu items def opts [("<cr>", withFocus pure)]
  where
    opts =
      scratch (ScratchId name) & #filetype ?~ name
    name =
      "myo-commands"

myoCommands ::
  Members (CommandMenuStack WindowMenu) r =>
  Members [Controller !! RunError, AtomicState CommandState, Async] r =>
  Handler r ()
myoCommands =
  resumeReports @[Controller, Rpc] @[_, _] $ mapReports @[RpcError, CommandError] do
    commandMenu >>= \case
      Success ident ->
        void (async (Controller.runIdent ident))
      Error err ->
        stop (CommandError.Misc err)
      _ ->
        unit
