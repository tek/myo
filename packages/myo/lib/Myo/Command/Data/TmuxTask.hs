module Myo.Command.Data.TmuxTask where

import Chiasma.Data.TmuxId (PaneId)

import Myo.Command.Data.Command (Command)

data TmuxTask =
  TmuxTask {
    pane :: PaneId,
    command :: Command
  }
  deriving stock (Eq, Show)
