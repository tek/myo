module Myo.Command.Command where

import Chiasma.Data.Ident (Ident, sameIdent)
import Control.Lens (Lens')
import qualified Control.Lens as Lens (views)
import Control.Monad.DeepError (MonadDeepError, hoistMaybe)
import Control.Monad.DeepState (MonadDeepState, gets)
import Data.Foldable (find)
import Ribosome.Control.Monad.Ribo (inspectHeadE)

import Myo.Command.Data.Command (Command(Command), CommandLanguage)
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandError as CommandError (CommandError(NoSuchCommand, NoCommands))
import Myo.Command.Data.CommandInterpreter (CommandInterpreter(System, Shell))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (commands, history)
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import qualified Myo.Command.Data.HistoryEntry as HistoryEntry (HistoryEntry(command))

mayCommandByIdent ::
  MonadDeepState s CommandState m =>
  Ident ->
  m (Maybe Command)
mayCommandByIdent ident =
  gets f
  where
    f :: CommandState -> Maybe Command
    f = Lens.views CommandState.commands (find $ sameIdent ident)

commandByIdent ::
  (MonadDeepError e CommandError m, MonadDeepState s CommandState m) =>
  Ident ->
  m Command
commandByIdent ident =
  hoistMaybe (CommandError.NoSuchCommand ident) =<< mayCommandByIdent ident

systemCommand :: Maybe Ident -> Ident -> [Text] -> Maybe Ident -> Maybe CommandLanguage -> Command
systemCommand target =
  Command (System target)

shellCommand :: Ident -> Ident -> [Text] -> Maybe Ident -> Maybe CommandLanguage -> Command
shellCommand target =
  Command (Shell target)

latestCommand ::
  (MonadDeepError e CommandError m, MonadDeepState s CommandState m) =>
  m Command
latestCommand =
  HistoryEntry.command <$> inspectHeadE CommandError.NoCommands l
  where
    l :: Lens' CommandState [HistoryEntry]
    l = CommandState.history

mainCommand :: Ident -> m Ident
mainCommand =
  undefined
