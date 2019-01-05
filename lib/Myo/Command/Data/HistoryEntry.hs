module Myo.Command.Data.HistoryEntry(
  HistoryEntry(..),
) where

import Myo.Command.Data.Command (Command)
import Myo.Data.Ident (Ident)

data HistoryEntry =
  HistoryEntry {
    command :: Command,
    target :: Maybe Ident
  }
  deriving (Eq, Show)
