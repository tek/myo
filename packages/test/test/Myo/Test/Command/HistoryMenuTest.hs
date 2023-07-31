module Myo.Test.Command.HistoryMenuTest where

import Polysemy.Test (UnitTest, assertEq, (===))
import Ribosome.Menu (MenuResult (Aborted), promptInput)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Prompt (PromptEvent (Mapping, Update))
import Ribosome.Test (testError)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.HistoryEntry (HistoryEntry (HistoryEntry))
import Myo.Command.History (history)
import Myo.Command.HistoryMenu (HistoryAction (Run), historyMenu)
import Myo.Data.CommandId (CommandId)
import Myo.Test.Embed (myoTest)

inputEvents :: [PromptEvent]
inputEvents =
  [Update "line", Mapping "k", Mapping "<cr>"]

mkEntry :: CommandId -> HistoryEntry
mkEntry n =
  HistoryEntry (Command.cons (CommandInterpreter.System Nothing) n mempty)

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
