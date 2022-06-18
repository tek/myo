module Myo.Output.Data.ParseResult where

import Chiasma.Data.Ident (Ident)

import Myo.Output.Data.ParsedOutput (ParsedOutput)

data ParseResult =
  ParseResult {
    _command :: Ident,
    _output :: [ParsedOutput]
  }
  deriving stock (Eq, Show)
