module Myo.Data.ProcessTask where

import Myo.Data.CommandId (CommandId)

data ProcessTask =
  ProcessTask {
    ident :: CommandId,
    cmd :: (String, [String])
  }
  deriving stock (Eq, Show)
