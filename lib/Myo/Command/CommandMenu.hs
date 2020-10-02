module Myo.Command.CommandMenu where

import qualified Chiasma.Data.Ident as Ident (Ident(..))
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Conduit (yield)
import qualified Control.Lens as Lens (view)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchSize)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Menu.Action (menuQuit, menuQuitWith)
import Ribosome.Menu.Data.Menu (Menu)
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import Ribosome.Menu.Data.MenuItem (simpleMenuItem)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Nvim (getCharC, nvimPromptRenderer)
import Ribosome.Menu.Prompt.Run (basicTransition)
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Menu.Simple (defaultMenu, selectedMenuItem)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Tmux.Run (RunTmux)

import Myo.Command.Data.Command (Command(Command))
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Run (myoRunIdent)
import Myo.Data.Env (Env)
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Render (MyoRender)

runCommand ::
  RunTmux m =>
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
runCommand menu _ =
  maybe (menuQuit menu) runQuit (Lens.view MenuItem.meta <$> selectedMenuItem menu)
  where
    runQuit ident =
      menuQuitWith (myoRunIdent ident) menu

menuItemName :: Ident -> Maybe Text -> Text
menuItemName ident displayName =
  "[" <> fromMaybe (text ident) displayName <> "]"
  where
    text (Ident.Str a) =
      toText a
    text (Ident.Uuid a) =
      Text.take 6 $ UUID.toText a

commandMenu ::
  NvimE e m =>
  MonadRibo m =>
  MonadResource m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e CommandError m =>
  (Menu Ident -> Prompt -> m (MenuConsumerAction m a, Menu Ident)) ->
  m (MenuResult a)
commandMenu execute =
  run =<< getL @CommandState CommandState.commands
  where
    run [] =
      throwHoist CommandError.NoCommands
    run entries =
      nvimMenu (scratchOptions (length entries)) (items entries) handler promptConfig Nothing
    items entries =
      yield (menuItem <$> entries)
    menuItem (Command _ ident lines' _ _ displayName _ _ _) =
      simpleMenuItem ident (menuItemText ident lines' displayName)
    menuItemText ident lines' displayName =
      Text.unwords [menuItemName ident displayName, Text.take 100 . fromMaybe "<no command line>" $ listToMaybe lines']
    handler =
      defaultMenu (Map.fromList [("cr", execute)])
    promptConfig =
      PromptConfig (getCharC 0.033) basicTransition nvimPromptRenderer []
    scratchOptions count =
      scratchSize count $ defaultScratchOptions "myo-commands"

myoCommands ::
  NvimE e m =>
  MonadRibo m =>
  MonadResource m =>
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
myoCommands =
  void $ commandMenu runCommand
