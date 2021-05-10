module Myo.Output.Data.EventIndex where

newtype Relative =
  Relative { unRelative :: Natural }
  deriving (Eq, Show)

instance Default Relative where
  def = Relative 0

newtype Absolute =
  Absolute { unAbsolute :: Natural }
  deriving (Eq, Show)

instance Default Absolute where
  def = Absolute 0
