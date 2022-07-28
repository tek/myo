module Myo.Command.Data.ProcPids where

import Process (Pid)

data ProcPids =
  Node Pid [ProcPids]
  |
  Leaf Pid
  deriving stock (Eq, Show)
