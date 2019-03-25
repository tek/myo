module Myo.Command.History where

import Control.Monad.DeepState (MonadDeepState)
import Ribosome.Control.Monad.Ribo (prepend)

import Myo.Command.Data.Command (Command)
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (history)
import Myo.Command.Data.HistoryEntry (HistoryEntry(HistoryEntry))

pushHistory ::
  ∀ s m.
  MonadDeepState s CommandState m =>
  Command ->
  m ()
pushHistory cmd =
  prepend @CommandState CommandState.history (HistoryEntry cmd)
