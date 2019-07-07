module Myo.Output.Data.EventIndex where

newtype Relative =
  Relative Natural
  deriving (Eq, Show)

instance Default Relative where
  def = Relative 0

newtype Absolute =
  Absolute Natural
  deriving (Eq, Show)

instance Default Absolute where
  def = Absolute 0
