module Myo.Data.ProcessTask where

import Chiasma.Data.Ident (Ident)

data ProcessTask =
  ProcessTask {
    ident :: Ident,
    cmd :: (String, [String])
  }
  deriving stock (Eq, Show)
