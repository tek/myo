module Myo.Output.Data.ReportLine where

data ReportLine a =
  ReportLine {
    _event :: a,
    _text :: Text
  }
  deriving (Eq, Show)
