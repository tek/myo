module Myo.Data.Maybe where

orFalse :: Maybe Bool -> Bool
orFalse =
  fromMaybe False
