module Myo.Command.History where

import Chiasma.Data.Ident (sameIdent)
import qualified Control.Lens as Lens (element, firstOf)
import Control.Monad.DeepError (hoistMaybe)
import Control.Monad.DeepState (MonadDeepState)

import Myo.Command.Data.Command (Command)
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandError as CommandError (CommandError(NoSuchHistoryIndex))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (history)
import Myo.Command.Data.HistoryEntry (HistoryEntry(HistoryEntry))
import qualified Myo.Command.Data.HistoryEntry as HistoryEntry (HistoryEntry(command))

pushHistory ::
  MonadDeepState s CommandState m =>
  Command ->
  m ()
pushHistory cmd =
  modifyL @CommandState CommandState.history prep
  where
    prep es = HistoryEntry cmd : filter (not . sameIdent cmd) es

lookupHistory ::
  MonadDeepState s CommandState m =>
  MonadDeepError e CommandError m =>
  Int ->
  m Command
lookupHistory index =
  HistoryEntry.command <$> (err =<< gets @CommandState (Lens.firstOf (CommandState.history . Lens.element index)))
  where
    err = hoistMaybe (CommandError.NoSuchHistoryIndex index)
