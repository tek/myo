module Myo.Command.Data.ExecutionState where
import Process (Pid)

data ExecutionState =
  Pending
  |
  Running
  |
  Starting Pid
  |
  Tracked Pid
  |
  Stopped
  |
  Unknown
  deriving stock (Eq, Show)
