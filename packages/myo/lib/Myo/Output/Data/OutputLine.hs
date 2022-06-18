module Myo.Output.Data.OutputLine where

data OutputLine =
  OutputLine {
     _text :: Text,
     _indent :: Maybe Int
  }
  deriving stock (Eq, Show)
