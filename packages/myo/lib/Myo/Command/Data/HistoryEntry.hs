module Myo.Command.Data.HistoryEntry where

import Chiasma.Data.Ident (Identifiable (..))
import Polysemy.Time.Json (json)

import Myo.Command.Data.Command (Command)

newtype HistoryEntry =
  HistoryEntry {
    command :: Command
  }
  deriving stock (Eq, Show, Generic)

json ''HistoryEntry

instance Identifiable HistoryEntry where
  identify =
    identify . command
