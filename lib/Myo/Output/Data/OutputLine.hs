module Myo.Output.Data.OutputLine where

data OutputLine =
  OutputLine {
     _text :: String,
     _indent :: Maybe Int
  }
  deriving (Eq, Show)
