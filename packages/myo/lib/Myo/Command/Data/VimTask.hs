module Myo.Command.Data.VimTask where

import Chiasma.Data.Ident (Ident)

data VimTask =
  VimTask {
    commands :: [Text],
    silent :: Bool,
    target :: Maybe Ident
  }
  deriving stock (Eq, Show)
