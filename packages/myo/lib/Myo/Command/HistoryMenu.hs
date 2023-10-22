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
  menuOk,
  menuRenderIndex,
  menuState,
  menuSuccess,
  staticWindowMenu,
  withFocus,
  withFocus',
  withInsert,
  withSelection',
  )
import qualified Ribosome.Report as Report

import Myo.Command.CommandMenu (cmdlineItem)
import Myo.Command.CommandSpec (compileTemplateWithDefaults)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.HistoryEntry
import Myo.Command.Data.HistoryEntry (ExecutionParams (ExecutionParams), HistoryEntry (HistoryEntry))
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Edit (EditItem, editHistoryEntry, editHistoryEntryCompiled)
import Myo.Command.Run (reRunAsync)
import Myo.Data.CommandId (CommandId)
import Myo.Effect.Controller (Controller)
import qualified Myo.Effect.History as History
import Myo.Effect.History (History)

data HistoryAction =
  Run CommandId
  |
  Delete (NonEmpty CommandId)
  |
  Edit CommandId
  |
  EditCompiled CommandId [Text]
  deriving stock (Eq, Show, Generic)

historyItem :: HistoryEntry -> MenuItem CommandId
historyItem entry =
  cmdlineItem ((.id) <$> entry.execution) entry.command ((.compiled) <$> entry.execution)

historyItems ::
  Members [History, Stop CommandError] r =>
  Sem r [MenuItem CommandId]
historyItems = do
  entries <- stopNote CommandError.NoHistory . nonEmpty =<< History.all
  pure (toList (historyItem <$> entries))

type HistoryMenuStack =
  [
    ModalWindowMenus CommandId !! RpcError,
    History !! RunError,
    Settings !! SettingError,
    Rpc !! RpcError,
    ReportLog,
    Log
  ]

delete ::
  Member History r =>
  MenuWidget (ModalState CommandId) r a
delete =
  menuState $ withSelection' \ idents -> do
    History.remove idents
    deleteSelected
    menuRenderIndex

edit :: MenuWidget (ModalState CommandId) r HistoryAction
edit =
  withFocus \ i -> pure (Edit i)

editCompiled ::
  Members [History, ReportLog] r =>
  MenuWidget (ModalState CommandId) r HistoryAction
editCompiled =
  withFocus' \ i -> do
    History.queryId i >>= \case
      HistoryEntry {execution = Just ExecutionParams {compiled}} ->
        menuSuccess (EditCompiled i compiled)
      HistoryEntry {command} ->
        case compileTemplateWithDefaults command of
          Right compiled -> menuSuccess (EditCompiled i compiled)
          Left err -> do
            Report.info "This command cannot be edited without parameters." ["Edit menu: Can't edit compiled", err]
            menuOk

historyMenu ::
  Members HistoryMenuStack r =>
  Members [Stop RunError, Stop RpcError] r =>
  Sem r (MenuResult HistoryAction)
historyMenu =
  restop @RunError @History do
    items <- mapStop RunError.Command historyItems
    staticWindowMenu items def opts actions
  where
    opts = def & #items .~ (scratch (ScratchId name) & #filetype ?~ name)
    name = "myo-history"

    actions =
      [
        (withInsert "<cr>", withFocus (pure . Run)),
        ("d", delete),
        ("e", edit),
        ("E", editCompiled)
      ]

-- TODO pretty sure the async part is from a time when the menu needed to be executed synchronously – the handler for
-- MyoHistory is Async, so it should be fine to run the command sync
--
-- The test should be fine to run this function directly then, I think.
--
-- Or maybe this function should only take care of the errors.
myoHistory ::
  Members HistoryMenuStack r =>
  Members [ModalWindowMenus EditItem !! RpcError, ReportLog] r =>
  Members [Controller !! RunError, Input Ident, Async] r =>
  Handler r ()
myoHistory =
  resumeReports @[Controller, Rpc] @[_, _] $ mapReports @[RpcError, CommandError, RunError] do
    historyMenu >>= \case
      Success (Run ident) ->
        reRunAsync (Left ident) mempty
      Success (Delete idents) ->
        restop @RunError (History.remove idents)
      Success (Edit ident) ->
        editHistoryEntry ident
      Success (EditCompiled ident cmdlines) ->
        editHistoryEntryCompiled ident cmdlines
      Error err ->
        stop (CommandError.Misc err)
      Aborted ->
        unit
