module Myo.Command.HistoryMenu where

import qualified Chiasma.Data.Ident as Ident
import Chiasma.Data.Ident (Ident)
import qualified Data.Text as Text (take, unwords)
import qualified Data.UUID as UUID (toText)
import Exon (exon)
import Ribosome (
  Handler,
  Rpc,
  RpcError,
  ScratchId (ScratchId),
  Settings,
  mapHandlerErrors,
  resumeHandlerErrors,
  scratch,
  )
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Menu (
  MenuItem,
  MenuResult (Error, Success),
  NvimMenu,
  menu,
  runStaticNvimMenu,
  withFocus,
  withMappings,
  )
import Ribosome.Menu.Data.MenuItem (simpleMenuItem)

import Myo.Command.Data.Command (Command (Command), cmdLines, displayName, ident)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.HistoryEntry (HistoryEntry (HistoryEntry))
import Myo.Command.Data.RunError (RunError)
import Myo.Command.History (history)
import Myo.Command.Run (reRun)
import Myo.Effect.Controller (Controller)

menuItemName :: Ident -> Maybe Text -> Text
menuItemName ident displayName =
  [exon|[#{fromMaybe (identTextShort ident) displayName}]|]
  where
    identTextShort = \case
      Ident.Str a ->
        a
      Ident.Uuid a ->
        Text.take 6 (UUID.toText a)

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

type HistoryMenuStack =
  NvimMenu Ident ++ [
    AtomicState CommandState,
    Settings !! SettingError,
    Rpc !! RpcError
  ]

historyMenu ::
  Members HistoryMenuStack r =>
  Members [Stop CommandError, Stop RpcError] r =>
  Sem r (MenuResult Ident)
historyMenu = do
  items <- historyItems
  runStaticNvimMenu items [] opts $ withMappings [("cr", withFocus pure)] menu
  where
    opts =
      scratch (ScratchId name) & #filetype ?~ name
    name =
      "myo-history"

myoHistory ::
  Members HistoryMenuStack r =>
  Members [Controller !! RunError, AtomicState CommandState, Async] r =>
  Handler r ()
myoHistory =
  resumeHandlerErrors @[Controller, Rpc] @[_, _] $ mapHandlerErrors @[RpcError, CommandError] do
    historyMenu >>= \case
      Success ident ->
        void (async (reRun (Left ident)))
      Error err ->
        stop (CommandError.Misc err)
      _ ->
        unit
