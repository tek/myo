module Myo.Command.Data.ExecutionState where

import Process (Pid)

data ExecutionState =
  Pending
  |
  Running
  |
  Tracked Pid
  |
  Stopped
  |
  Unknown
  deriving stock (Eq, Show)
