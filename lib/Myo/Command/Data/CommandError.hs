module Myo.Command.Data.CommandError(
  CommandError(..),
) where

import Chiasma.Data.Ident (Ident)

newtype CommandError =
  CommandError Ident
  deriving (Eq, Show)
