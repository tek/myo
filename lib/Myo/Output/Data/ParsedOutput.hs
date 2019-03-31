module Myo.Output.Data.ParsedOutput where

import Data.DeepLenses (deepLenses)
import Ribosome.Data.Syntax (Syntax)

import Myo.Output.Data.ParseReport (ParseReport)

data ParsedOutput =
  ParsedOutput {
    _syntax :: Syntax,
    _report :: Int -> ParseReport
    }
