module Myo.Command.Data.Pid(
  Pid(..),
) where

newtype Pid =
  Pid Int
  deriving (Eq, Show)
