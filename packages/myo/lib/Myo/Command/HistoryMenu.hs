module Myo.Command.HistoryMenu where

import qualified Data.Text as Text (take, unwords)
import Ribosome (Handler, Rpc, RpcError, ScratchId (ScratchId), Settings, mapReports, resumeReports, scratch)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Menu (
  MenuAction (Render),
  MenuItem,
  MenuResult (Error, Success),
  MenuWidget,
  ModalState,
  ModalWindowMenus,
  WindowMenu,
  deleteSelected,
  menuState,
  staticWindowMenu,
  withFocus,
  withSelection',
  )
import Ribosome.Menu.Data.MenuItem (simpleMenuItem)

import Myo.Command.CommandMenu (menuItemName)
import Myo.Command.Data.Command (Command (Command), cmdLines, displayName, ident)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.HistoryEntry (HistoryEntry (HistoryEntry))
import Myo.Command.Data.RunError (RunError)
import Myo.Command.History (history, removeHistoryEntries)
import Myo.Command.Run (reRun)
import Myo.Data.CommandId (CommandId)
import Myo.Effect.Controller (Controller)

data HistoryAction =
  Run CommandId
  |
  Delete (NonEmpty CommandId)
  deriving stock (Eq, Show, Generic)

historyItems ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Sem r [MenuItem CommandId]
historyItems = do
  entries <- stopNote CommandError.NoHistory . nonEmpty =<< history
  pure (toList (menuItem <$> entries))
  where
    menuItem (HistoryEntry (Command {ident, cmdLines, displayName})) =
      simpleMenuItem ident (menuItemText ident cmdLines displayName)
    menuItemText ident lines' displayName =
      Text.unwords [menuItemName ident displayName, Text.take 100 . fromMaybe "<no command line>" $ listToMaybe lines']

type HistoryMenuStack ui =
  [
    ModalWindowMenus CommandId !! RpcError,
    AtomicState CommandState,
    Settings !! SettingError,
    Rpc !! RpcError,
    Log
  ]

delete ::
  Member (AtomicState CommandState) r =>
  MenuWidget (ModalState CommandId) r a
delete =
  menuState $ withSelection' \ idents -> do
    removeHistoryEntries idents
    deleteSelected
    pure Render

historyMenu ::
  Members (HistoryMenuStack ui) r =>
  Members [Stop CommandError, Stop RpcError] r =>
  Sem r (MenuResult HistoryAction)
historyMenu = do
  items <- historyItems
  staticWindowMenu items def opts [("<cr>", withFocus (pure . Run)), ("d", delete)]
  where
    opts =
      def & #items .~ (scratch (ScratchId name) & #filetype ?~ name)
    name =
      "myo-history"

myoHistory ::
  Members (HistoryMenuStack WindowMenu) r =>
  Members [Controller !! RunError, AtomicState CommandState, Async] r =>
  Handler r ()
myoHistory =
  resumeReports @[Controller, Rpc] @[_, _] $ mapReports @[RpcError, CommandError] do
    historyMenu >>= \case
      Success (Run ident) ->
        void (async (reRun (Left ident)))
      Success (Delete idents) ->
        removeHistoryEntries idents
      Error err ->
        stop (CommandError.Misc err)
      _ ->
        unit
