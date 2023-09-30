module Myo.Command.HistoryMenu where

import Chiasma.Data.Ident (Ident)
import Ribosome (Handler, ReportLog, Rpc, RpcError, ScratchId (ScratchId), Settings, mapReports, resumeReports, scratch)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Menu (
  MenuItem,
  MenuResult (Aborted, Error, Success),
  MenuWidget,
  ModalState,
  ModalWindowMenus,
  deleteSelected,
  menuRenderIndex,
  menuState,
  staticWindowMenu,
  withFocus,
  withSelection',
  )

import Myo.Command.CommandMenu (cmdlineItem)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.HistoryEntry
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Edit (EditItem, editHistoryEntry)
import Myo.Command.History (history, removeHistoryEntries)
import Myo.Command.Run (reRunAsync)
import Myo.Data.CommandId (CommandId)
import Myo.Effect.Controller (Controller)

data HistoryAction =
  Run CommandId
  |
  Delete (NonEmpty CommandId)
  |
  Edit CommandId
  deriving stock (Eq, Show, Generic)

historyItem :: HistoryEntry -> MenuItem CommandId
historyItem entry =
  cmdlineItem ((.id) <$> entry.execution) entry.command ((.compiled) <$> entry.execution)

historyItems ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Sem r [MenuItem CommandId]
historyItems = do
  entries <- stopNote CommandError.NoHistory . nonEmpty =<< history
  pure (toList (historyItem <$> entries))

type HistoryMenuStack =
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
    menuRenderIndex

edit :: MenuWidget (ModalState CommandId) r HistoryAction
edit =
  withFocus \ i -> pure (Edit i)

historyMenu ::
  Members HistoryMenuStack r =>
  Members [Stop CommandError, Stop RpcError] r =>
  Sem r (MenuResult HistoryAction)
historyMenu = do
  items <- historyItems
  staticWindowMenu items def opts [("<cr>", withFocus (pure . Run)), ("d", delete), ("e", edit)]
  where
    opts =
      def & #items .~ (scratch (ScratchId name) & #filetype ?~ name)
    name =
      "myo-history"

-- TODO pretty sure the async part is from a time when the menu needed to be executed synchronously – the handler for
-- MyoHistory is Async, so it should be fine to run the command sync
--
-- The test should be fine to run this function directly then, I think.
--
-- Or maybe this function should only take care of the errors.
myoHistory ::
  Members HistoryMenuStack r =>
  Members [ModalWindowMenus EditItem !! RpcError, ReportLog] r =>
  Members [Controller !! RunError, AtomicState CommandState, Input Ident, Async] r =>
  Handler r ()
myoHistory =
  resumeReports @[Controller, Rpc] @[_, _] $ mapReports @[RpcError, CommandError] do
    historyMenu >>= \case
      Success (Run ident) ->
        reRunAsync (Left ident) mempty
      Success (Delete idents) ->
        removeHistoryEntries idents
      Success (Edit ident) ->
        editHistoryEntry ident
      Error err ->
        stop (CommandError.Misc err)
      Aborted ->
        unit
