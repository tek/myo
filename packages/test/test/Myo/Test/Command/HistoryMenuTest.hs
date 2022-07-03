module Myo.Test.Command.HistoryMenuTest where

import Chiasma.Data.Ident (Ident)
import Polysemy.Test (UnitTest, (===))
import Ribosome.Menu (MenuRead, MenuWidget, interpretMenu, menuRead)
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Items.Read (withFocusItem)
import Ribosome.Menu.Prompt (interpretPromptInputCharList)
import Ribosome.Test (testError)

import qualified Myo.Command.Data.Command as Command
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.HistoryEntry (HistoryEntry (HistoryEntry))
import Myo.Command.HistoryMenu (historyMenu)
import Myo.Test.Embed (myoTest)

inputChars :: [Text]
inputChars =
  ["i", "l", "i", "n", "e", "esc", "k", "cr"]

history :: [HistoryEntry]
history =
  [entry "c1", entry "c2", entry "c3", entry "c4", entry "c4", entry "c4", entry "c4", entry "c4"]
  where
    entry n =
      HistoryEntry (Command.cons (CommandInterpreter.System Nothing) n mempty)

exec ::
  MenuRead Ident r =>
  MenuWidget r Text
exec =
  Just . maybe MenuAction.abort MenuAction.success <$> menuRead (withFocusItem (pure . MenuItem.text))

test_historyMenu :: UnitTest
test_historyMenu =
  myoTest do
    atomicSet @CommandState #history history
    entry <- interpretMenu $ interpretPromptInputCharList inputChars do
      testError (historyMenu exec)
    MenuResult.Success "[c2] <no command line>" === entry
