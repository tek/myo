module Myo.Data.ProcError where

newtype ProcError =
  ProcError { unProcError :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)
