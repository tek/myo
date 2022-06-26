module Myo.Command.Data.CommandOutput where

data CommandOutput =
  CommandOutput {
    history :: [Text],
    current :: Seq Text
  }
  deriving stock (Eq, Show)
