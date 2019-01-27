module Myo.Command.Data.HistoryEntry(
  HistoryEntry(..),
) where

import Myo.Command.Data.Command (Command)
import Chiasma.Data.Ident (Ident)

data HistoryEntry =
  HistoryEntry {
    command :: Command,
    target :: Maybe Ident
  }
  deriving (Eq, Show)
