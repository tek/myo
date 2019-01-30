module Myo.Command.Data.RunError(
  RunError(..),
) where

import Myo.Command.Data.CommandError (CommandError)

data RunError =
  RunError String
  |
  Command CommandError
  deriving (Eq, Show)
