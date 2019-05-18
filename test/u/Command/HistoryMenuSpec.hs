{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Command.HistoryMenuSpec (htf_thisModulesTests) where

import Control.Concurrent.Lifted (fork, killThread)
import Control.Exception.Lifted (bracket)
import Control.Lens ((^?))
import qualified Control.Lens as Lens (element)
import qualified Data.Text as Text (unlines)
import Ribosome.Api.Variable (setVar)
import Ribosome.Menu.Data.Menu (Menu(Menu))
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (MenuItem(text))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Simple (menuQuit)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand, vimInput)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)
import Test.Framework

import Myo.Command.Data.Command (Command(Command))
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter (CommandInterpreter(System))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (history)
import Myo.Command.Data.HistoryEntry (HistoryEntry(HistoryEntry))
import Myo.Command.HistoryMenu (historyMenu)
import Myo.Data.Env (Myo)

defineLoopFunction :: Myo ()
defineLoopFunction = do
  setVar "looping" True
  vimCommand $ Text.unlines ["function! Loop()", "while g:looping", "sleep 100m", "endwhile", "endfunction"]

nativeChars :: [Text]
nativeChars =
  ["k", "<cr>"]

history :: [HistoryEntry]
history =
  [entry "c1", entry "c2", entry "c3"]
  where
    entry n =
      HistoryEntry (Command (CommandInterpreter.System Nothing) n def def def)

exec ::
  MonadIO m =>
  MVar (Maybe Text) ->
  Menu ->
  Prompt ->
  m (MenuConsumerAction m, Menu)
exec var m@(Menu _ items _ selected _) _ =
  swapMVar var (MenuItem.text <$> item) *> menuQuit m
  where
    item =
      items ^? Lens.element selected

historyMenuSpec :: Myo ()
historyMenuSpec = do
  setL @CommandState CommandState.history history
  defineLoopFunction
  () <- vimCommand "call feedkeys(\":call Loop()\\<cr>\")"
  var <- newMVar Nothing
  bracket (fork input) cleanup (const (historyMenu (exec var)))
  gassertEqual (Just "[c2] <no command line>") =<< liftIO (readMVar var)
  where
    input = do
      sleep 0.1
      traverse_ vimInput nativeChars
    cleanup inputThreadId =
      setVar "looping" False *>
      killThread inputThreadId

test_historyMenu :: IO ()
test_historyMenu =
  tmuxGuiSpecDef historyMenuSpec
