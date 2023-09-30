module Myo.Command.CommandMenu where

import qualified Chiasma.Data.Ident as Ident
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Exon (exon)
import Ribosome (Handler, Rpc, RpcError, ScratchId (ScratchId), Settings, mapReports, resumeReports, scratch)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Menu (
  MenuItem,
  MenuResult (Error, Success),
  ModalWindowMenus,
  WindowMenu,
  simpleMenuItem,
  staticWindowMenu,
  withFocus,
  )

import qualified Myo.Command.Data.Command
import Myo.Command.Data.Command (Command, ident)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandSpec (renderCommandSpec)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Run (runIdentAsync)
import Myo.Data.CommandId (CommandId (CommandId))
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

cmdlineItem :: Maybe CommandId -> Command -> Maybe [Text] -> MenuItem CommandId
cmdlineItem idOverride command compiled =
  simpleMenuItem ident menuItemText
  where
    ident = fromMaybe command.ident idOverride

    menuItemText =
      Text.unwords [
        menuItemName ident command.displayName,
        Text.take 100 (fromMaybe "<no command line>" (head cmdline))
      ]

    cmdline = fromMaybe (renderCommandSpec command.cmdLines) compiled

commandItem :: Command -> MenuItem CommandId
commandItem command = cmdlineItem Nothing command Nothing

commandItems ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Sem r [MenuItem CommandId]
commandItems = do
  cmds <- stopNote CommandError.NoCommands . nonEmpty =<< atomicView #commands
  pure (toList (commandItem <$> cmds))

type CommandMenuStack ui =
  [
    ModalWindowMenus CommandId !! RpcError,
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
  staticWindowMenu items def opts [("<cr>", withFocus pure)]
  where
    opts =
      def & #items .~ (scratch (ScratchId name) & #filetype ?~ name)
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
        runIdentAsync ident mempty
      Error err ->
        stop (CommandError.Misc err)
      _ ->
        unit
