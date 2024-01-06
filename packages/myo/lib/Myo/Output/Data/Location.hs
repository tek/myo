module Myo.Output.Data.Location where

data Location =
  Location {
    path :: Text,
    line :: Int,
    col :: Maybe Int
  }
  deriving stock (Eq, Show)
