module Myo.Command.Data.HistoryEntry(
  HistoryEntry(..),
) where

import Myo.Command.Data.Command (Command)

newtype HistoryEntry =
  HistoryEntry {
    command :: Command
  }
  deriving (Eq, Show)
