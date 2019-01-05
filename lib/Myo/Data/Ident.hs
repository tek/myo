module Myo.Data.Ident(
  Ident(..),
) where

import Data.UUID (UUID)

data Ident =
  Str String
  |
  Uuid UUID
  deriving (Eq, Show)
