module Myo.Command.HistoryMenu where

import qualified Chiasma.Data.Ident as Ident (Ident(..))
import Conduit (yieldMany)
import qualified Control.Lens as Lens (view)
import Control.Monad.Catch (MonadThrow)
import qualified Data.Map as Map (fromList)
import qualified Data.Text as Text (take, unwords)
import qualified Data.UUID as UUID (toText)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (ident)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Nvim (getCharC, nvimPromptRenderer)
import Ribosome.Menu.Prompt.Run (basicTransition)
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Menu.Simple (defaultMenu, menuQuit, menuQuitWith, selectedMenuItem)
import Ribosome.Msgpack.Error (DecodeError)

import Chiasma.Ui.Data.TreeModError (TreeModError)
import Myo.Command.Data.Command (Command(Command))
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandError as CommandError (CommandError(NoHistory))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.HistoryEntry (HistoryEntry(HistoryEntry))
import Myo.Command.Data.RunError (RunError)
import Myo.Command.History (history)
import Myo.Command.Run (myoReRun)
import Myo.Data.Env (Env)
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Render (MyoRender)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Tmux.Run (RunTmux)

runHistoryEntry ::
  Monad m =>
  MonadRibo m =>
  RunTmux m =>
  MyoRender s e m =>
  MonadBaseControl IO m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e PersistError m =>
  MonadDeepError e RunError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ToggleError m =>
  MonadDeepError e TreeModError m =>
  MonadDeepState s CommandState m =>
  MonadDeepState s Env m =>
  MonadThrow m =>
  Menu ->
  Prompt ->
  m (MenuConsumerAction m (), Menu)
runHistoryEntry menu _ =
  maybe (menuQuit menu) runQuit (Lens.view MenuItem.ident <$> selectedMenuItem menu)
  where
    runQuit ident =
      menuQuitWith (myoReRun (Left (Ident.Str (toString ident)))) menu


menuItemName :: Ident -> Maybe Text -> Text
menuItemName ident displayName =
  "[" <> fromMaybe (text ident) displayName <> "]"
  where
    text (Ident.Str a) =
      toText a
    text (Ident.Uuid a) =
      Text.take 6 $ UUID.toText a

historyMenu ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e CommandError m =>
  (Menu -> Prompt -> m (MenuConsumerAction m a, Menu)) ->
  m (MenuResult a)
historyMenu execute =
  run =<< history
  where
    run [] =
      throwHoist CommandError.NoHistory
    run entries =
      nvimMenu def (items entries) handler promptConfig
    items entries =
      yieldMany (menuItem <$> entries)
    menuItem (HistoryEntry (Command _ ident lines' _ _ displayName)) =
      MenuItem (identText ident) (menuItemText ident lines' displayName)
    menuItemText ident lines' displayName =
      Text.unwords [menuItemName ident displayName, Text.take 50 . fromMaybe "<no command line>" $ listToMaybe lines']
    handler =
      defaultMenu (Map.fromList [("cr", execute)])
    promptConfig =
      PromptConfig (getCharC 0.033) basicTransition nvimPromptRenderer False

myoHistory ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  RunTmux m =>
  MyoRender s e m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e PersistError m =>
  MonadDeepError e RunError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e ToggleError m =>
  MonadDeepError e TreeModError m =>
  MonadDeepState s CommandState m =>
  MonadDeepState s Env m =>
  MonadThrow m =>
  m ()
myoHistory =
  void $ historyMenu runHistoryEntry
