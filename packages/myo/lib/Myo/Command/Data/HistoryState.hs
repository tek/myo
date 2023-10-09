module Myo.Command.Data.HistoryState where

import Myo.Command.Data.HistoryEntry (HistoryEntry)

data HistoryState =
  HistoryState {
    history :: [HistoryEntry]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)
