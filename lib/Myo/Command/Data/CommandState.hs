module Myo.Command.Data.CommandState(
  CommandState(..),
) where

import GHC.Generics (Generic)
import Data.Default.Class (Default)
import Myo.Command.Data.Command (Command)
import Myo.Command.Data.HistoryEntry (HistoryEntry)

data CommandState =
  CommandState {
    commands :: [Command],
    history :: [HistoryEntry]
  }
  deriving (Eq, Show, Generic, Default)
