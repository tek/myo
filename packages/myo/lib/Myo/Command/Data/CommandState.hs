module Myo.Command.Data.CommandState where

import Exon (exon)
import Prelude hiding (output)

import Myo.Command.Data.Command (Command)
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Data.OutputState (OutputState)

data CommandState =
  CommandState {
    commands :: [Command],
    history :: [HistoryEntry],
    output :: Maybe OutputState
  }
  deriving stock (Generic)
  deriving anyclass (Default)

instance Show CommandState where
  showsPrec d CommandState {..} =
    showParen (d > 10) [exon|CommandState #{showsPrec 11 commands} #{showsPrec 11 history} #{showsPrec 11 output}|]
