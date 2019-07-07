module Myo.Output.Data.ParseResult where

import Myo.Output.Data.ParsedOutput (ParsedOutput)

data ParseResult =
  ParseResult {
    _command :: Ident,
    _output :: [ParsedOutput]
  }

makeClassy ''ParseResult
