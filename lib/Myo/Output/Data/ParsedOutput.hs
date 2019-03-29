module Myo.Output.Data.ParsedOutput where

import Data.DeepLenses (deepLenses)

import Myo.Output.Data.ParseReport (ParseReport)

newtype ParsedOutput =
  ParsedOutput (Int -> ParseReport)
