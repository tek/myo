module Myo.Output.Data.ReportLine where

import Data.Text (Text)

data ReportLine =
  ReportLine {
    _event :: Int,
    _text :: Text
  }
  deriving (Eq, Show)
