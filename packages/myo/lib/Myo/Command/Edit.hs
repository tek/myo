module Myo.Command.Edit where

import Chiasma.Data.Ident (Ident)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Exon (exon)
import Ribosome (Report, ReportLog, RpcError, ScratchId (ScratchId), scratch)
import Ribosome.Menu (
  MenuApp,
  MenuItem (..),
  MenuResult (Aborted, Error, Success),
  MenuWidget,
  ModalState,
  ModalWindowMenus,
  PromptText (PromptText),
  menuAttachPrompt,
  menuDetachPrompt,
  menuOk,
  menuState,
  menuSuccess,
  modifyFocus',
  notPrompt,
  onlyPrompt,
  staticWindowMenu,
  use,
  withFocus',
  )
import qualified Ribosome.Menu.Data.Entry
import Ribosome.Menu.Data.Entry (Entry (Entry))
import qualified Ribosome.Menu.Data.MenuItem
import Ribosome.Menu.MenuState (items)
import qualified Ribosome.Menu.Prompt.Data.Prompt
import qualified Ribosome.Report as Report

import qualified Myo.Command.Data.Command
import Myo.Command.Data.Command (Command)
import Myo.Command.Data.CommandError (CommandError (InvalidTemplateEdit, Misc))
import qualified Myo.Command.Data.CommandSpec
import Myo.Command.Data.CommandSpec (CommandSpec (CommandSpec))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandTemplate
import Myo.Command.Data.CommandTemplate (CommandTemplate, parseCommandTemplate)
import qualified Myo.Command.Data.HistoryEntry
import Myo.Command.Data.HistoryEntry (HistoryEntry (HistoryEntry))
import qualified Myo.Command.Data.Param
import Myo.Command.Data.Param (
  ParamId (ParamId),
  ParamValue (ParamFlag, ParamValue),
  ParamValues,
  parseParamFlag,
  renderParamValue,
  )
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Edit.Syntax (editSyntax)
import Myo.Command.History (lookupHistoryIdent)
import Myo.Command.Run (reRun, runCommand)
import Myo.Data.CommandId (CommandId (CommandId))
import Myo.Effect.Controller (Controller)

data EditItem =
  Param ParamId ParamValue
  |
  Cmdline Int Text
  deriving stock (Eq, Show, Generic)

data EditAction =
  Run
  |
  Save
  deriving stock (Eq, Show, Generic)

data EditResult =
  EditResult {
    action :: EditAction,
    items :: [MenuItem EditItem]
  }
  deriving stock (Eq, Show, Generic)

paramValues :: HistoryEntry -> [(ParamId, ParamValue)]
paramValues = \case
  HistoryEntry {execution = Just exe} -> Map.toList exe.params
  HistoryEntry {command} -> Map.toList (coerce <$> command.cmdLines.params)

paramItem :: Int -> ParamId -> ParamValue -> MenuItem EditItem
paramItem width (ParamId i) v =
  MenuItem (Param (ParamId i) v) text [text]
  where
    text = [exon| 🛠 #{padding}#{i}: #{renderParamValue v}|]
    padding = Text.replicate (max 0 (width - Text.length i)) " "

cmdlineItem :: Int -> Bool -> Int -> Text -> MenuItem EditItem
cmdlineItem width single index cline =
  MenuItem (Cmdline index cline) text [text]
  where
    text = [exon| 🟢 #{padding}#{desc}: ##{cline}|]
    padding = Text.replicate (max 0 (width - Text.length desc)) " "
    desc = [exon|cmdline#{indexIndicator}|]
    indexIndicator | single = ""
                   | otherwise = [exon| #{show index}|]

editItems :: HistoryEntry -> [MenuItem EditItem]
editItems entry =
  reverse (uncurry (paramItem width) <$> values) <>
  reverse (zipWith (cmdlineItem width single) [0..] cmdlines)
  where
    width = fromMaybe cmdlineWidth (maximum (cmdlineWidth : valueWidths))
    cmdlineWidth | single = 7
                 | otherwise = 9
    single = length cmdlines == 1
    cmdlines = entry.command.cmdLines.template.rendered
    valueWidths = values <&> \ (ParamId i, _) -> Text.length i
    values = paramValues entry

itemValue :: EditItem -> Text
itemValue = \case
  Param _ value -> renderParamValue value
  Cmdline _ text -> text

edit :: MenuWidget (ModalState EditItem) r a
edit =
  withFocus' \ item ->
    menuAttachPrompt (Just (fromText (itemValue item)))

updateEditItem :: Text -> EditItem -> Either Text EditItem
updateEditItem new = \case
  Param pid (ParamValue _) ->
    Right (Param pid (fromText new))
  Param pid (ParamFlag _) ->
    Param pid <$> parseParamFlag new
  Cmdline number _ ->
    Right (Cmdline number (fromText new))

updateItem :: Text -> MenuItem EditItem -> Either Text (MenuItem EditItem)
updateItem new MenuItem {meta} =
  updateEditItem new meta <&> \ newMeta -> MenuItem {meta = newMeta, text = new, render = [new]}

update ::
  Member ReportLog r =>
  MenuWidget (ModalState EditItem) r a
update = do
  PromptText newValue <- asks (.prompt.text)
  result <- modifyFocus' \ old@Entry {..} -> do
    case updateItem newValue item of
      Right new -> (Entry {item = new, ..}, Nothing)
      Left err -> (old, Just err)
  case join result of
    Just err -> do
      Report.info err ["Edit menu: item update failed", err]
      menuOk
    Nothing ->
      menuDetachPrompt (Just "")

finish :: EditAction -> MenuWidget (ModalState EditItem) r EditResult
finish action =
  menuState do
    its <- use items
    menuSuccess (EditResult action (toList its))

newCommandSpec :: [MenuItem EditItem] -> Either Text (CommandTemplate, ParamValues)
newCommandSpec newItems = do
  template <- parseCommandTemplate (Right (snd <$> sortOn fst cmdlines))
  pure (template, Map.fromList params)
  where
    (cmdlines, params) = partitionEithers (extractItem . (.meta) <$> newItems)
    extractItem = \case
      Param pid value -> Right (pid, value)
      Cmdline num value -> Left (num, value)

updateCommand ::
  Member (Input Ident) r =>
  CommandTemplate ->
  Command ->
  Sem r (Maybe Command)
updateCommand template command
  | template == command.cmdLines.template
  = pure Nothing
  | otherwise
  = do
    newId <- CommandId <$> input
    pure (Just (command & #ident .~ newId & #cmdLines .~ CommandSpec template mempty))

runAction ::
  Members [Controller !! RunError, AtomicState CommandState, Stop CommandError, Stop Report] r =>
  Command ->
  ParamValues ->
  EditAction ->
  Bool ->
  Sem r ()
runAction command newParams action changed
  | Run <- action
  , changed
  = runCommand command newParams
  | Run <- action
  = reRun (Left command.ident) (Just newParams)
  | otherwise
  = stop (Misc "Save not implemented")

handleAction ::
  Members [Controller !! RunError, AtomicState CommandState, Input Ident, Stop CommandError, Stop Report] r =>
  HistoryEntry ->
  MenuResult EditResult ->
  Sem r ()
handleAction entry = \case
  Success (EditResult action newItems) -> do
    (newTemplate, newParams) <- stopEitherWith (InvalidTemplateEdit (show newItems)) (newCommandSpec newItems)
    updateCommand newTemplate entry.command >>= \case
      Just newCommand -> runAction newCommand newParams action True
      Nothing -> runAction entry.command newParams action False
  Aborted ->
    unit
  Error err ->
    stop (Misc err)

type EditMenuStack =
  [
    ModalWindowMenus EditItem !! RpcError,
    Controller !! RunError,
    AtomicState CommandState,
    Input Ident,
    Stop CommandError,
    Stop Report,
    ReportLog,
    Log
  ]

app ::
  Member ReportLog r =>
  MenuApp (ModalState EditItem) r EditResult
app =
  [
    ("r", finish Run),
    ("s", finish Save),
    (notPrompt "<cr>", edit),
    (onlyPrompt "<cr>", update)
  ]

-- TODO could have another mode triggered by a separate mapping in the history menu that doesn't include params and
-- operates only on the interpolated command
--
-- TODO edit also runner and other options?
editHistoryEntryMenu ::
  Members EditMenuStack r =>
  Members [Stop CommandError, Stop RpcError] r =>
  CommandId ->
  Sem r (HistoryEntry, MenuResult EditResult)
editHistoryEntryMenu cid = do
  entry <- lookupHistoryIdent cid
  result <- staticWindowMenu (editItems entry) def opts app
  pure (entry, result)
  where
    opts =
      def & #items .~ (scratch (ScratchId name) & #filetype ?~ name & #syntax .~ [editSyntax])
    name =
      "myo-command-edit"

editHistoryEntry ::
  Members EditMenuStack r =>
  Members [Stop CommandError, Stop RpcError] r =>
  CommandId ->
  Sem r ()
editHistoryEntry cid = do
  (entry, result) <- editHistoryEntryMenu cid
  handleAction entry result
