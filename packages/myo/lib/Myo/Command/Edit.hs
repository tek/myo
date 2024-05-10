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
  MenuSem,
  MenuWidget,
  ModalState,
  ModalWindowMenus,
  PromptState (PromptState),
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
  withInsert,
  )
import qualified Ribosome.Menu.Data.Entry
import Ribosome.Menu.Data.Entry (Entry (Entry))
import qualified Ribosome.Menu.Data.MenuItem
import Ribosome.Menu.MenuState (items)
import Ribosome.Menu.Prompt (PromptControl (PromptControlApp))
import qualified Ribosome.Menu.Prompt.Data.Prompt
import qualified Ribosome.Report as Report

import qualified Myo.Command.Data.Command
import Myo.Command.Data.Command (Command)
import Myo.Command.Data.CommandError (CommandError (InvalidTemplate, Misc))
import qualified Myo.Command.Data.CommandSpec
import Myo.Command.Data.CommandSpec (CommandSpec (CommandSpec))
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
import Myo.Command.Run (reRun, runCommand)
import Myo.Data.CommandId (CommandId (CommandId))
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)
import Myo.Effect.Controller (Controller)
import qualified Myo.Effect.History as History
import Myo.Effect.History (History)

data EditItem =
  Param ParamId ParamValue
  |
  Cmdline Int Text
  deriving stock (Eq, Show, Generic)

data EditAction =
  Run { history :: Bool }
  |
  Save
  deriving stock (Eq, Show, Generic)

data EditResult =
  EditResult {
    action :: EditAction,
    items :: [MenuItem EditItem]
  }
  deriving stock (Eq, Show, Generic)

data LayoutParams =
  LayoutParams {
    width :: Int,
    single :: Bool
  }
  deriving stock (Eq, Show, Generic)

paramValues :: HistoryEntry -> ParamValues
paramValues = \case
  HistoryEntry {execution = Just exe} -> exe.params
  HistoryEntry {command} -> coerce command.cmdLines.params

paramItem :: LayoutParams -> ParamId -> ParamValue -> MenuItem EditItem
paramItem params (ParamId i) v =
  MenuItem (Param (ParamId i) v) text [text]
  where
    text = [exon| 🛠 #{padding}#{i}: #{renderParamValue v}|]
    padding = Text.replicate (max 0 (params.width - Text.length i)) " "

cmdlineItem :: LayoutParams -> Int -> Text -> MenuItem EditItem
cmdlineItem LayoutParams {width, single} index cline =
  MenuItem (Cmdline index cline) text [text]
  where
    text = [exon| ✏️ #{padding}#{desc}: ##{cline}|]
    padding = Text.replicate (max 0 (width - Text.length desc)) " "
    desc = [exon|cmdline#{indexIndicator}|]
    indexIndicator | single = ""
                   | otherwise = [exon| #{show index}|]

menuItems :: Command -> ParamValues -> ([MenuItem EditItem], LayoutParams)
menuItems command (Map.toList -> values) =
  (its, params)
  where
    its =
      reverse (uncurry (paramItem params) <$> values) <>
      reverse (zipWith (cmdlineItem params) [0..] cmdlines)

    params = LayoutParams {..}

    single = length cmdlines == 1

    width = fromMaybe cmdlineWidth (maximum (cmdlineWidth : valueWidths))

    cmdlineWidth | single = 7
                 | otherwise = 9

    cmdlines = command.cmdLines.template.rendered

    valueWidths = values <&> \ (ParamId i, _) -> Text.length i

menuItems_compiled :: [Text] -> ([MenuItem EditItem], LayoutParams)
menuItems_compiled cmdlines =
  (reverse (zipWith (cmdlineItem params) [0..] cmdlines), params)
  where
    params = LayoutParams {..}

    width | single = 7
          | otherwise = 9
    single = length cmdlines == 1

itemValue :: EditItem -> Text
itemValue = \case
  Param _ value -> renderParamValue value
  Cmdline _ text -> text

edit :: MenuWidget (ModalState EditItem) r a
edit =
  withFocus' \ item ->
    menuAttachPrompt (Just (fromText (itemValue item)))

updateEditItem :: LayoutParams -> Text -> EditItem -> Either Text (MenuItem EditItem)
updateEditItem params new = \case
  Param pid (ParamValue _) ->
    Right (paramItem params pid (fromText new))
  Param pid (ParamFlag _) ->
    paramItem params pid <$> parseParamFlag new
  Cmdline number _ ->
    Right (cmdlineItem params number (fromText new))

updateItem :: LayoutParams -> Text -> MenuItem EditItem -> Either Text (MenuItem EditItem)
updateItem params new MenuItem {meta} =
  updateEditItem params new meta

tryUpdate ::
  Member ReportLog r =>
  LayoutParams ->
  MenuSem (ModalState EditItem) r Bool
tryUpdate params = do
  PromptText newValue <- asks (.prompt.text)
  result <- modifyFocus' \ old@Entry {..} -> do
    case updateItem params newValue item of
      Right new -> (Entry {item = new, ..}, Nothing)
      Left err -> (old, Just err)
  case join result of
    Just err -> do
      Report.info err ["Edit menu: item update failed", err]
      pure False
    Nothing ->
      pure True

update ::
  Member ReportLog r =>
  LayoutParams ->
  MenuWidget (ModalState EditItem) r a
update params =
  tryUpdate params >>= \case
    True -> menuDetachPrompt (Just "")
    False -> menuOk

finish ::
  Member ReportLog r =>
  LayoutParams ->
  EditAction ->
  MenuWidget (ModalState EditItem) r EditResult
finish params action = do
  success <- ask >>= \case
    PromptState {control = PromptControlApp} -> tryUpdate params
    _ -> pure True
  if success
  then menuState do
    its <- use items
    menuSuccess (EditResult action (toList its))
  else menuOk

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
  Members [Controller !! RunError, History !! RunError, Stop CommandError, Stop Report] r =>
  Command ->
  ParamValues ->
  EditAction ->
  Bool ->
  Sem r ()
runAction command newParams action changed
  | Run { history = True } <- action
  , not changed
  = reRun (Left command.ident) (Just newParams) Nothing
  | Run {} <- action
  = runCommand command newParams Nothing
  | otherwise
  = stop (Misc "Save not implemented")

handleAction ::
  Members [Controller !! RunError, History !! RunError, Input Ident, Stop CommandError, Stop Report] r =>
  Command ->
  MenuResult EditResult ->
  Sem r ()
handleAction command = \case
  Success (EditResult action newItems) -> do
    (newTemplate, newParams) <- stopEitherWith (InvalidTemplate True (show newItems)) (newCommandSpec newItems)
    updateCommand newTemplate command >>= \case
      Just newCommand -> runAction newCommand newParams action True
      Nothing -> runAction command newParams action False
  Aborted ->
    unit
  Error err ->
    stop (Misc err)

type EditMenuStack =
  [
    ModalWindowMenus EditItem !! RpcError,
    Controller !! RunError,
    History !! RunError,
    Input Ident,
    Stop RunError,
    Stop CommandError,
    Stop RpcError,
    Stop Report,
    ReportLog,
    Log
  ]

app ::
  Member ReportLog r =>
  Bool ->
  LayoutParams ->
  MenuApp (ModalState EditItem) r EditResult
app history params =
  [
    (withInsert "<cr>", finish params (Run history)),
    (withInsert "<c-s>", finish params Save),
    (notPrompt "e", edit),
    (onlyPrompt "<esc>", update params)
  ]

editCommandItems ::
  Members EditMenuStack r =>
  Bool ->
  [MenuItem EditItem] ->
  LayoutParams ->
  Sem r (MenuResult EditResult)
editCommandItems history its params =
  staticWindowMenu its def opts (app history params)
  where
    opts = def & #items .~ (scratch (ScratchId name) & #filetype ?~ name & #syntax .~ [editSyntax])
    name = "myo-command-edit"

-- TODO edit also runner and other options?
editCommandMenu ::
  Members EditMenuStack r =>
  Bool ->
  Command ->
  ParamValues ->
  Sem r (MenuResult EditResult)
editCommandMenu history command values = do
  let (its, params) = menuItems command values
  editCommandItems history its params

editCommand ::
  Members EditMenuStack r =>
  Member (Commands !! CommandError) r =>
  CommandId ->
  Sem r ()
editCommand cid = do
  command <- restop (Commands.queryId cid)
  result <- editCommandMenu False command (coerce command.cmdLines.params)
  handleAction command result

editHistoryEntry ::
  Members EditMenuStack r =>
  CommandId ->
  Sem r ()
editHistoryEntry cid = do
  entry <- restop (History.queryId cid)
  result <- editCommandMenu True entry.command (paramValues entry)
  handleAction entry.command result

editHistoryEntryCompiled ::
  Members EditMenuStack r =>
  CommandId ->
  [Text] ->
  Sem r ()
editHistoryEntryCompiled cid cmdlines = do
  entry <- restop (History.queryId cid)
  let (its, params) = menuItems_compiled cmdlines
  result <- editCommandItems True its params
  handleAction entry.command result
