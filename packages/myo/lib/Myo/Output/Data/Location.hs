module Myo.Output.Data.Location where

data Location =
  Location {
    _path :: Text,
    _line :: Int,
    _col :: Maybe Int
  }
  deriving stock (Eq, Show)
