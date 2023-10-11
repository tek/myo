module Myo.Data.ProcessTask where

import Myo.Data.CommandId (CommandId)

data ProcessTask =
  ProcessTask {
    ident :: CommandId,
    cmd :: (String, [String]),
    maxLogBytes :: Maybe Int
  }
  deriving stock (Eq, Show)
