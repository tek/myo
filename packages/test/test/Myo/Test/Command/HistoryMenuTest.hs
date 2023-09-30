module Myo.Test.Command.HistoryMenuTest where

import Chiasma.Data.Ident (Ident (Str))
import Polysemy.Test (TestError (TestError), UnitTest, assertEq, evalMaybe, (/==), (===))
import Ribosome (interpretPersistNull)
import Ribosome.Api (nvimGetVar)
import Ribosome.Menu (MenuResult (Aborted, Success), promptInput)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Prompt (PromptEvent (Mapping, Update))
import Ribosome.Test (testError)

import Myo.Command.CommandSpec (compileTemplateWith)
import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter
import Myo.Command.Data.CommandSpec (CommandSpec (CommandSpec))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.CommandTemplate (parseCommandTemplate')
import qualified Myo.Command.Data.HistoryEntry
import Myo.Command.Data.HistoryEntry (ExecutionParams (ExecutionParams), HistoryEntry (HistoryEntry))
import Myo.Command.Data.Param (ParamDefault (ParamDefault), ParamValues)
import Myo.Command.Edit (editHistoryEntryMenu, handleAction)
import Myo.Command.History (history)
import Myo.Command.HistoryMenu (HistoryAction (Edit, Run), historyMenu)
import Myo.Command.Interpreter.Backend.Vim (interpretBackendVim)
import Myo.Command.Interpreter.CommandLog (interpretCommandLogSetting)
import Myo.Data.CommandId (CommandId (CommandId))
import Myo.Interpreter.Controller (interpretController)
import Myo.Test.Embed (myoTest)

inputEvents :: [PromptEvent]
inputEvents =
  [Update "line", Mapping "k", Mapping "<cr>"]

mkEntry :: CommandId -> HistoryEntry
mkEntry n =
  HistoryEntry (Command.cons (CommandInterpreter.System Nothing) n mempty) Nothing

initialHistory :: [HistoryEntry]
initialHistory =
  [mkEntry "c1", mkEntry "c2", mkEntry "c3", mkEntry "c4", mkEntry "c5", mkEntry "c6", mkEntry "c7", mkEntry "c8"]

test_historyMenu :: UnitTest
test_historyMenu =
  myoTest do
    atomicSet @CommandState #history initialHistory
    entry <- promptInput inputEvents do
      testError @CommandError historyMenu
    MenuResult.Success (Run "c2") === entry

inputEventsDelete :: [PromptEvent]
inputEventsDelete =
  [
    Mapping "k",
    Mapping "<space>",
    Mapping "k",
    Mapping "k",
    Mapping "<space>",
    Mapping "<space>",
    Mapping "d",
    Mapping "d",
    Mapping "<esc>"
  ]

historyDeleted :: [HistoryEntry]
historyDeleted =
  [mkEntry "c1", mkEntry "c3", mkEntry "c4", mkEntry "c8"]

test_historyMenuDelete :: UnitTest
test_historyMenuDelete =
  myoTest do
    atomicSet @CommandState #history initialHistory
    result <- promptInput inputEventsDelete (testError @CommandError historyMenu)
    Aborted === result
    assertEq historyDeleted =<< history

params :: ParamValues
params =
  [("par1", "value 1"), ("par2", "value 2")]

originalId :: CommandId
originalId =
  CommandId (Str "exe1")

historyEdit ::
  Member (Error TestError) r =>
  Sem r [HistoryEntry]
historyEdit = do
  compiled <- fromEither (first TestError (compileTemplateWith tpl params))
  pure [HistoryEntry cmd (Just (ExecutionParams originalId compiled params))]
  where
    cmd = Command.consSpec (CommandInterpreter.Vim False Nothing) "1" spec
    spec = CommandSpec tpl (coerce params)
    tpl = parseCommandTemplate' ["garbage"]

editEvents :: [PromptEvent]
editEvents =
  [
    Mapping "<cr>",
    Update "new value",
    Mapping "<cr>",
    Mapping "k",
    Mapping "k",
    Mapping "<cr>",
    Update "let g:command = '{par1} {par2} \\{par3\\}'",
    Mapping "<cr>",
    Mapping "r"
  ]

test_historyMenuEdit :: UnitTest
test_historyMenuEdit =
  myoTest $ interpretPersistNull $ interpretCommandLogSetting $ interpretBackendVim $ interpretController $
  testError @CommandError do
    atomicSet @CommandState #history =<< historyEdit
    Success (Edit i) <- promptInput [Mapping "e"] do
      historyMenu
    (entry, action) <- promptInput editEvents do
      editHistoryEntryMenu i
    handleAction entry action
    value <- nvimGetVar "command"
    ("value 1 new value {par3}" :: Text) === value
    newId <- evalMaybe =<< head . fmap (.command.ident) <$> history
    originalId /== newId
