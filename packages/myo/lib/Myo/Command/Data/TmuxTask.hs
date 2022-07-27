module Myo.Command.Data.TmuxTask where

import Chiasma.Data.TmuxId (PaneId)

import Myo.Command.Data.Command (Command)
import Myo.Command.Data.UiTarget (UiTarget)

data TaskType =
  Shell
  |
  Wait
  |
  Kill
  deriving stock (Eq, Show)

data TmuxTask =
  TmuxTask {
    taskType :: TaskType,
    target :: UiTarget,
    pane :: PaneId,
    command :: Command
  }
  deriving stock (Eq, Show)
