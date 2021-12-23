module Myo.Command.CommandMenu where

import qualified Chiasma.Data.Ident as Ident (Ident (..))
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.ScratchOptions (defaultScratchOptions)
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Menu.Consumer as Consumer
import Ribosome.Menu.Data.MenuConsumer (MenuWidget)
import Ribosome.Menu.Data.MenuItem (simpleMenuItem)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import Ribosome.Menu.Items.Read (withFocusM)
import Ribosome.Menu.Prompt (defaultPrompt)
import Ribosome.Menu.Run (staticNvimMenu)
import Ribosome.Msgpack.Error (DecodeError)

import Myo.Command.Data.Command (Command (Command))
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandState as CommandState
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Run (myoRunIdent)
import Myo.Data.Env (Env)
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Render (MyoRender)

runCommand ::
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
  MenuWidget m Ident ()
runCommand =
  withFocusM myoRunIdent

menuItemName :: Ident -> Maybe Text -> Text
menuItemName ident displayName =
  "[" <> fromMaybe (text' ident) displayName <> "]"
  where
    text' (Ident.Str a) =
      toText a
    text' (Ident.Uuid a) =
      Text.take 6 $ UUID.toText a

commandMenu ::
  NvimE e m =>
  MonadRibo m =>
  MonadCatch m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e CommandError m =>
  MenuWidget m Ident () ->
  m (MenuResult ())
commandMenu execute =
  run . fmap menuItem =<< getL @CommandState CommandState.commands
  where
    run [] =
      throwHoist CommandError.NoCommands
    run entries =
      staticNvimMenu scratchOptions entries handler promptConfig
    menuItem (Command _ ident lines' _ _ displayName _ _ _) =
      simpleMenuItem ident (menuItemText ident lines' displayName)
    menuItemText ident lines' displayName =
      Text.unwords [menuItemName ident displayName, Text.take 100 . fromMaybe "<no command line>" $ listToMaybe lines']
    handler =
      Consumer.withMappings (Map.fromList [("cr", execute)])
    promptConfig =
      defaultPrompt []
    scratchOptions =
      defaultScratchOptions "myo-commands"

myoCommands ::
  NvimE e m =>
  MonadCatch m =>
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
  m ()
myoCommands =
  void $ commandMenu runCommand
