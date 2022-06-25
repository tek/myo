module Myo.Command.Data.Pid where

newtype Pid =
  Pid { unPid :: Int }
  deriving stock (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord)
