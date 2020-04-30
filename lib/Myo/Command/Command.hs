module Myo.Command.Command where

import Chiasma.Data.Ident (Ident)
import qualified Chiasma.Data.Ident as Ident
import Control.Lens (Lens')
import qualified Control.Lens as Lens (filtered, firstOf, folded, view, views)
import Control.Monad.DeepError (MonadDeepError, hoistMaybe)
import Control.Monad.DeepState (MonadDeepState, gets)
import Ribosome.Control.Monad.Ribo (inspectHeadE)

import Myo.Command.Data.Command (Command(Command), CommandLanguage)
import qualified Myo.Command.Data.Command as Command (displayName, ident, interpreter)
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandError as CommandError (CommandError(NoSuchCommand, NoCommands))
import Myo.Command.Data.CommandInterpreter (CommandInterpreter(System, Shell))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (commands, history)
import qualified Myo.Command.Data.HistoryEntry as HistoryEntry (command)

mayCommandBy ::
  Eq a =>
  MonadDeepState s CommandState m =>
  Lens' Command a ->
  a ->
  m (Maybe Command)
mayCommandBy lens a =
  gets f
  where
    f :: CommandState -> Maybe Command
    f = Lens.firstOf (CommandState.commands . Lens.folded . Lens.filtered (Lens.views lens (a ==)))

noSuchCommand ::
  MonadDeepError e CommandError m =>
  Text ->
  Text ->
  Maybe a ->
  m a
noSuchCommand context query =
  hoistMaybe (CommandError.NoSuchCommand context query)

commandBy ::
  Eq a =>
  MonadDeepError e CommandError m =>
  MonadDeepState s CommandState m =>
  Text ->
  Text ->
  Lens' Command a ->
  a ->
  m Command
commandBy context ident lens a =
  noSuchCommand context ident =<< mayCommandBy lens a

mayCommandByIdent ::
  MonadDeepState s CommandState m =>
  Ident ->
  m (Maybe Command)
mayCommandByIdent =
  mayCommandBy Command.ident

commandByIdent ::
  MonadDeepError e CommandError m =>
  MonadDeepState s CommandState m =>
  Text ->
  Ident ->
  m Command
commandByIdent context ident =
  commandBy context (show ident) Command.ident ident

commandByName ::
  MonadDeepError e CommandError m =>
  MonadDeepState s CommandState m =>
  Text ->
  Text ->
  m Command
commandByName context name =
  commandBy context name Command.displayName (Just name)

commandByIdentOrName ::
  MonadDeepError e CommandError m =>
  MonadDeepState s CommandState m =>
  Text ->
  Text ->
  m Command
commandByIdentOrName context query = do
  byIdent <- mayCommandByIdent (Ident.Str query)
  byName <- mayCommandBy Command.displayName (Just query)
  noSuchCommand context query (byIdent <|> byName)

systemCommand :: Maybe Ident -> Ident -> [Text] -> Maybe Ident -> Maybe CommandLanguage -> Maybe Text -> Bool -> Command
systemCommand target =
  Command (System target)

shellCommand :: Ident -> Ident -> [Text] -> Maybe Ident -> Maybe CommandLanguage -> Maybe Text -> Bool -> Command
shellCommand target =
  Command (Shell target)

latestCommand ::
  (MonadDeepError e CommandError m, MonadDeepState s CommandState m) =>
  m Command
latestCommand =
  Lens.view HistoryEntry.command <$> inspectHeadE @CommandState CommandError.NoCommands CommandState.history

mainCommand ::
  MonadDeepError e CommandError m =>
  MonadDeepState s CommandState m =>
  Ident ->
  m Ident
mainCommand ident =
  recurse =<< Lens.view Command.interpreter <$$> mayCommandByIdent ident
  where
    recurse (Just (Shell target)) =
      mainCommand target
    recurse _ =
      return ident
