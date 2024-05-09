module Myo.Test.Command.HistoryMenuTest where

import Chiasma.Data.Ident (Ident (Str))
import Polysemy.Test (TestError (TestError), UnitTest, assertEq, evalMaybe, (/==), (===))
import Ribosome (interpretPersistNull)
import Ribosome.Api (nvimGetVar)
import Ribosome.Menu (MenuResult (Aborted, Success), promptInput)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Prompt (PromptEvent (Mapping, Update))
import Ribosome.Test (resumeTestError, testError)

import Myo.Command.CommandSpec (compileTemplateWith)
import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter
import Myo.Command.Data.CommandSpec (CommandSpec (CommandSpec))
import Myo.Command.Data.CommandTemplate (parseCommandTemplate')
import qualified Myo.Command.Data.HistoryEntry
import Myo.Command.Data.HistoryEntry (ExecutionParams (ExecutionParams), HistoryEntry (HistoryEntry))
import Myo.Command.Data.Param (ParamDefault (ParamDefault), ParamValues)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Edit (editHistoryEntryMenu, handleAction)
import Myo.Command.HistoryMenu (HistoryAction (Edit, Run), historyMenu)
import Myo.Command.Interpreter.Backend.Vim (interpretBackendVim)
import Myo.Data.CommandId (CommandId (CommandId))
import qualified Myo.Effect.History as History
import Myo.Effect.History (History)
import Myo.Interpreter.Commands (interpretCommands)
import Myo.Interpreter.Controller (interpretController)
import Myo.Interpreter.History (interpretHistoryTransient, interpretHistoryWith)
import Myo.Test.Embed (myoTest, myoTest)

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
  myoTest $ interpretPersistNull $ interpretHistoryWith initialHistory do
    entry <- promptInput inputEvents do
      testError @RunError historyMenu
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
  myoTest $ interpretPersistNull $ interpretHistoryWith initialHistory do
    result <- promptInput inputEventsDelete (testError @RunError historyMenu)
    Aborted === result
    assertEq historyDeleted =<< resumeTestError History.all

params :: ParamValues
params =
  [("par1", "value 1"), ("par2", "value 2")]

exeId :: CommandId
exeId =
  CommandId (Str "exe1")

historyEdit ::
  Member (Error TestError) r =>
  Sem r [HistoryEntry]
historyEdit = do
  compiled <- fromEither (first (TestError . show) (compileTemplateWith tpl mempty params))
  pure [HistoryEntry cmd (Just (ExecutionParams exeId compiled params))]
  where
    cmd = Command.consSpec (CommandInterpreter.Vim False Nothing) "1" spec
    spec = CommandSpec tpl (coerce params)
    tpl = parseCommandTemplate' ["garbage"]

editEvents1 :: [PromptEvent]
editEvents1 =
  [
    Mapping "e",
    Update "new value",
    Mapping "<esc>",
    Mapping "k",
    Mapping "k",
    Mapping "e",
    Update "let g:command = '{par1} {par2} \\{par3\\}'",
    Mapping "<cr>"
  ]

editEvents2 :: [PromptEvent]
editEvents2 =
  [
    Mapping "e",
    Update "third value",
    Mapping "<cr>"
  ]

test_historyMenuEdit :: UnitTest
test_historyMenuEdit =
  myoTest do
    initHistory <- historyEdit
    interpretBackendVim $ interpretHistoryTransient initHistory $
      interpretCommands $ interpretController $ testError @CommandError $ testError @RunError do
        value1 <- editWith editEvents1
        ("value 1 new value {par3}" :: Text) === value1
        newId <- evalMaybe =<< head . fmap (.command.ident) <$> resumeTestError @History History.all
        exeId /== newId
        value2 <- editWith editEvents2
        ("value 1 third value {par3}" :: Text) === value2
  where
    editWith events = do
      Success (Edit i) <- promptInput [Mapping "e"] do
        historyMenu
      (entry, action) <- promptInput events do
        editHistoryEntryMenu i
      handleAction entry action
      nvimGetVar "command"
