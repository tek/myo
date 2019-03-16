module Myo.Command.History(
  pushHistory,
) where

import Control.Lens (Lens')
import Control.Monad.State.Class (MonadState)
import Ribosome.Control.Monad.Ribo (prepend)

import Myo.Command.Data.Command (Command)
import qualified Myo.Command.Data.CommandState as CommandState (history)
import Myo.Command.Data.HistoryEntry (HistoryEntry(HistoryEntry))
import Myo.Data.Env (Env)
import qualified Myo.Data.Env as Env (command)

historyLens :: Lens' Env [HistoryEntry]
historyLens =
  Env.command . CommandState.history

pushHistory ::
  MonadState Env m =>
  Command ->
  m ()
pushHistory cmd =
  prepend historyLens (HistoryEntry cmd)
