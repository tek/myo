module Myo.Command.Data.Pid where

newtype Pid =
  Pid { pidNum :: Int }
  deriving stock (Eq, Show)
