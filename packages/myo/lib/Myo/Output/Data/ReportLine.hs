module Myo.Output.Data.ReportLine where

data ReportLine a =
  ReportLine {
    event :: a,
    text :: Text
  }
  deriving stock (Eq, Show)
