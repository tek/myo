module Myo.Command.History(
  pushHistory,
) where

import Control.Lens (Lens')
import Control.Monad.State.Class (MonadState)
import Ribosome.Control.Monad.State (prepend)

import Myo.Command.Data.Command (Command)
import qualified Myo.Command.Data.CommandState as CommandState (_history)
import Myo.Command.Data.HistoryEntry (HistoryEntry(HistoryEntry))
import Myo.Data.Env (Env)
import qualified Myo.Data.Env as Env (_command)

historyLens :: Lens' Env [HistoryEntry]
historyLens =
  Env._command . CommandState._history

pushHistory ::
  MonadState Env m =>
  Command ->
  m ()
pushHistory cmd =
  prepend historyLens (HistoryEntry cmd)
