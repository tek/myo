module Myo.Command.Data.HistoryEntry where

import Chiasma.Data.Ident (Identifiable(..))
import Myo.Command.Data.Command (Command)

newtype HistoryEntry =
  HistoryEntry {
    command :: Command
  }
  deriving (Eq, Show)

instance Identifiable HistoryEntry where
  identify = identify . command
