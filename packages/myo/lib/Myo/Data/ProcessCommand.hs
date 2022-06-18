module Myo.Data.ProcessCommand where

import Chiasma.Data.Ident (Ident)

data ProcessCommand =
  ProcessCommand {
    ident :: Ident,
    cmd :: (String, [String])
  }
  deriving stock (Eq, Show)
