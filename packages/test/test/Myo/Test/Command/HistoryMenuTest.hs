module Myo.Test.Command.HistoryMenuTest where

import Polysemy.Test (UnitTest, (===))
import Ribosome.Menu (promptInput)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Prompt (PromptEvent (Mapping, Update))
import Ribosome.Test (testError)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.HistoryEntry (HistoryEntry (HistoryEntry))
import Myo.Command.HistoryMenu (historyMenu)
import Myo.Test.Embed (myoTest)

inputEvents :: [PromptEvent]
inputEvents =
  [Update "line", Mapping "k", Mapping "<cr>"]

history :: [HistoryEntry]
history =
  [entry "c1", entry "c2", entry "c3", entry "c4", entry "c4", entry "c4", entry "c4", entry "c4"]
  where
    entry n =
      HistoryEntry (Command.cons (CommandInterpreter.System Nothing) n mempty)

test_historyMenu :: UnitTest
test_historyMenu =
  myoTest do
    atomicSet @CommandState #history history
    entry <- promptInput inputEvents do
      testError @CommandError historyMenu
    MenuResult.Success "c2" === entry
