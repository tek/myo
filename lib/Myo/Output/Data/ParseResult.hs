
module Myo.Output.Data.ParseResult where

import Chiasma.Data.Ident (Ident)
import Data.DeepLenses (deepLenses)

import Myo.Output.Data.ParsedOutput (ParsedOutput)

data ParseResult =
  ParseResult {
    _command :: Ident,
    _output :: [ParsedOutput]
  }

deepLenses ''ParseResult
