module Myo.Command.Data.HistoryEntry where

import Chiasma.Data.Ident (Identifiable(..))
import Data.Aeson (FromJSON, ToJSON(toEncoding), defaultOptions, genericToEncoding)

import Myo.Command.Data.Command (Command)

newtype HistoryEntry =
  HistoryEntry {
    _command :: Command
  }
  deriving (Eq, Show, Generic)

makeClassy ''HistoryEntry

instance Identifiable HistoryEntry where
  identify = identify . _command

instance ToJSON HistoryEntry where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON HistoryEntry
