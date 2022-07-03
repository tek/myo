module Myo.Command.HistoryMenu where

import qualified Chiasma.Data.Ident as Ident (Ident (..))
import Chiasma.Data.Ident (Ident)
import Conc (Restoration)
import qualified Data.Text as Text (take, unwords)
import qualified Data.UUID as UUID (toText)
import Polysemy.Chronos (ChronosTime)
import Ribosome (Handler, Rpc, RpcError, Scratch, Settings, mapHandlerError, resumeHandlerError)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Errors (pluginHandlerErrors)
import Ribosome.Menu (MenuItem, MenuStack, MenuWidget, MenuWrite, PromptInput, PromptListening, PromptQuit, interpretMenu, runNvimMenu, staticNvimMenu, withFocus, withMappings)
import Ribosome.Menu.Data.MenuItem (simpleMenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Prompt (interpretPromptInputNvim)

import Myo.Command.Data.Command (Command (Command), cmdLines, displayName, ident)
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandError as CommandError (CommandError (NoHistory))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.HistoryEntry (HistoryEntry (HistoryEntry))
import Myo.Command.Data.RunError (RunError)
import Myo.Command.History (history)
import Myo.Command.Run (reRun)
import Myo.Effect.Controller (Controller)

runHistoryEntry ::
  MenuWrite Ident r =>
  Members [Controller, AtomicState CommandState, Stop CommandError] r =>
  MenuWidget r ()
runHistoryEntry =
  withFocus (reRun . Left)

menuItemName :: Ident -> Maybe Text -> Text
menuItemName ident displayName =
  "[" <> fromMaybe (text' ident) displayName <> "]"
  where
    text' (Ident.Str a) =
      toText a
    text' (Ident.Uuid a) =
      Text.take 6 $ UUID.toText a

historyItems ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Sem r [MenuItem Ident]
historyItems = do
  entries <- stopNote CommandError.NoHistory . nonEmpty =<< history
  pure (toList (menuItem <$> entries))
  where
    menuItem (HistoryEntry (Command {ident, cmdLines, displayName})) =
      simpleMenuItem ident (menuItemText ident cmdLines displayName)
    menuItemText ident lines' displayName =
      Text.unwords [menuItemName ident displayName, Text.take 100 . fromMaybe "<no command line>" $ listToMaybe lines']

historyMenu ::
  Show a =>
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
  MenuWidget r a ->
  Sem r (MenuResult a)
historyMenu execute = do
  items <- historyItems
  withMappings [("cr", execute)] (runNvimMenu (staticNvimMenu items & #scratch . #name .~ "myo-history"))

myoHistory ::
  Members [Controller !! RunError, Scratch !! RpcError, Settings !! SettingError, AtomicState CommandState] r =>
  Members [Rpc !! RpcError, ChronosTime, Log, Resource, Race, Mask Restoration, Embed IO, Final IO] r =>
  Handler r ()
myoHistory =
  pluginHandlerErrors $ resumeHandlerError @Controller $ mapHandlerError do
    void $ interpretMenu $ interpretPromptInputNvim $ historyMenu runHistoryEntry
