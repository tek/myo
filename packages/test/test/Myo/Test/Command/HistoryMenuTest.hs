module Myo.Test.Command.HistoryMenuTest where

import Control.Concurrent.Lifted (fork, killThread)
import Control.Exception.Lifted (bracket)
import Ribosome.Api.Input (syntheticInput)
import qualified Ribosome.Menu.Data.MenuAction as MenuAction
import Ribosome.Menu.Data.MenuConsumer (MenuWidget)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuState (menuRead)
import Ribosome.Menu.Items.Read (withFocusItem)

import Myo.Command.Data.Command (Command (Command))
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter (CommandInterpreter (System))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (history)
import Myo.Command.Data.HistoryEntry (HistoryEntry (HistoryEntry))
import Myo.Command.HistoryMenu (historyMenu)

nativeChars :: [Text]
nativeChars =
  ["i", "l", "i", "n", "e", "<esc>", "k", "<cr>"]

history :: [HistoryEntry]
history =
  [entry "c1", entry "c2", entry "c3", entry "c4", entry "c4", entry "c4", entry "c4", entry "c4"]
  where
    entry n =
      HistoryEntry (Command (CommandInterpreter.System Nothing) n def def def def False False False)

exec ::
  MonadIO m =>
  MenuWidget m i Text
exec =
  Just . maybe MenuAction.abort (MenuAction.success . pure) <$> menuRead (withFocusItem (pure . MenuItem._text))

historyMenuTest :: Sem r ()
historyMenuTest = do
  setL @CommandState CommandState.history history
  entry <- bracket (fork input) killThread (const (historyMenu exec))
  MenuResult.Success "[c2] <no command line>" === entry
  where
    input =
      syntheticInput (Just 0.1) nativeChars

test_historyMenu :: UnitTest
test_historyMenu =
  tmuxTestDef historyMenuTest
