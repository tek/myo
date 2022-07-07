module Myo.Command.CommandMenu where

import Chiasma.Data.Ident (Ident (Str, Uuid))
import Conc (Restoration)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Exon (exon)
import Polysemy.Chronos (ChronosTime)
import Ribosome (Handler, Rpc, RpcError, Scratch, Settings, mapHandlerError, resumeHandlerError)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Errors (pluginHandlerErrors)
import Ribosome.Menu (
  MenuItem,
  MenuResult (Error, Success),
  MenuStack,
  PromptInput,
  PromptListening,
  PromptQuit,
  interpretMenu,
  runNvimMenu,
  simpleMenuItem,
  staticNvimMenu,
  withFocus,
  withMappings,
  )
import Ribosome.Menu.Prompt (interpretPromptInputNvim)

import Myo.Command.Data.Command (Command (Command, cmdLines, displayName), ident)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Effect.Controller as Controller
import Myo.Effect.Controller (Controller)

menuItemName :: Ident -> Maybe Text -> Text
menuItemName ident displayName =
  [exon|[#{fromMaybe (idText ident) displayName}]|]
  where
    idText = \case
      Str a ->
        a
      Uuid a ->
        Text.take 6 (UUID.toText a)

commandItems ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Sem r [MenuItem Ident]
commandItems = do
  cmds <- stopNote CommandError.NoCommands . nonEmpty =<< atomicView #commands
  pure (toList (menuItem <$> cmds))
  where
    menuItem Command {..} =
      simpleMenuItem ident (menuItemText ident cmdLines displayName)
    menuItemText ident ls displayName =
      Text.unwords [menuItemName ident displayName, Text.take 100 (fromMaybe "<no command line>" (head ls))]

commandMenu ::
  Members (MenuStack Ident) r =>
  Members [AtomicState CommandState, Stop CommandError] r =>
  Members [
    PromptInput,
    Settings !! SettingError,
    Scratch,
    Rpc,
    Rpc !! RpcError,
    Sync PromptQuit,
    Sync PromptListening,
    Log,
    ChronosTime,
    Mask res,
    Race,
    Embed IO,
    Final IO
  ] r =>
  Sem r (MenuResult Ident)
commandMenu = do
  items <- commandItems
  withMappings [("cr", withFocus pure)] (runNvimMenu (staticNvimMenu items & #scratch . #name .~ "myo-commands"))

myoCommands ::
  Members [Controller !! RunError, Scratch !! RpcError, Settings !! SettingError, AtomicState CommandState] r =>
  Members [Rpc !! RpcError, ChronosTime, Log, Resource, Race, Mask Restoration, Async, Embed IO, Final IO] r =>
  Handler r ()
myoCommands =
  pluginHandlerErrors $ resumeHandlerError @Controller $ mapHandlerError do
    interpretMenu $ interpretPromptInputNvim $ commandMenu >>= \case
      Success ident ->
        void (async (Controller.runIdent ident))
      Error err ->
        stop (CommandError.Misc err)
      _ ->
        unit
