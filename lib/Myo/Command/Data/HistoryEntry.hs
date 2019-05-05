module Myo.Command.Data.HistoryEntry where

import Chiasma.Data.Ident (Identifiable(..))
import Data.Aeson (FromJSON, ToJSON(toEncoding), defaultOptions, genericToEncoding)
import GHC.Generics (Generic)

import Myo.Command.Data.Command (Command)

newtype HistoryEntry =
  HistoryEntry {
    command :: Command
  }
  deriving (Eq, Show, Generic)

instance Identifiable HistoryEntry where
  identify = identify . command

instance ToJSON HistoryEntry where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON HistoryEntry
