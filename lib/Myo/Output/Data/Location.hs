module Myo.Output.Data.Location where

data Location =
  Location {
    _path :: FilePath,
    _line :: Int,
    _col :: Maybe Int
  }
  deriving (Eq, Show)
