module Myo.Output.Data.OutputLine where

import Data.DeepLenses (deepLenses)

data OutputLine =
  OutputLine {
     _text :: String,
     _indent :: Maybe Int
  }
  deriving (Eq, Show)
