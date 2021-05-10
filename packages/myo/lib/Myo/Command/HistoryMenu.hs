module Myo.Command.HistoryMenu where

import qualified Chiasma.Data.Ident as Ident (Ident(..))
import Conduit (yield)
import qualified Control.Lens as Lens (view)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Map as Map (fromList)
import qualified Data.Text as Text (take, unwords)
import qualified Data.UUID as UUID (toText)
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchSize)
import Ribosome.Menu.Action (menuQuit, menuQuitWith)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import Ribosome.Menu.Data.MenuItem (simpleMenuItem)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (meta)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Nvim (getCharC, nvimPromptRenderer)
import Ribosome.Menu.Prompt.Run (basicTransition)
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Menu.Simple (defaultMenu, selectedMenuItem)
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

runHistoryEntry ::
  MonadRibo m =>
  MonadThrow m =>
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
  Menu Ident ->
  Prompt ->
  m (MenuConsumerAction m (), Menu Ident)
runHistoryEntry menu _ =
  maybe (menuQuit menu) runQuit (Lens.view MenuItem.meta <$> selectedMenuItem menu)
  where
    runQuit ident =
      menuQuitWith (myoReRun (Left ident)) menu

menuItemName :: Ident -> Maybe Text -> Text
menuItemName ident displayName =
  "[" <> fromMaybe (text' ident) displayName <> "]"
  where
    text' (Ident.Str a) =
      toText a
    text' (Ident.Uuid a) =
      Text.take 6 $ UUID.toText a

historyMenu ::
  NvimE e m =>
  MonadRibo m =>
  MonadResource m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e CommandError m =>
  (Menu Ident -> Prompt -> m (MenuConsumerAction m a, Menu Ident)) ->
  m (MenuResult a)
historyMenu execute =
  run =<< history
  where
    run [] =
      throwHoist CommandError.NoHistory
    run entries =
      nvimMenu (scratchOptions (length entries)) (items entries) handler promptConfig Nothing
    items entries =
      yield (menuItem <$> entries)
    menuItem (HistoryEntry (Command _ ident lines' _ _ displayName _ _ _)) =
      simpleMenuItem ident (menuItemText ident lines' displayName)
    menuItemText ident lines' displayName =
      Text.unwords [menuItemName ident displayName, Text.take 100 . fromMaybe "<no command line>" $ listToMaybe lines']
    handler =
      defaultMenu (Map.fromList [("cr", execute)])
    promptConfig =
      PromptConfig (getCharC 0.033) basicTransition nvimPromptRenderer []
    scratchOptions count =
      scratchSize count $ defaultScratchOptions "myo-history"

myoHistory ::
  NvimE e m =>
  MonadResource m =>
  MonadBaseControl IO m =>
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
