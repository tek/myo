module Myo.Command.Data.HistoryEntry where

import Myo.Command.Data.Command (Command)

newtype HistoryEntry =
  HistoryEntry {
    command :: Command
  }
  deriving stock (Eq, Show, Generic)

unaryJson ''HistoryEntry
