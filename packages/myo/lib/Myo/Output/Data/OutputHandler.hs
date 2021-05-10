module Myo.Output.Data.OutputHandler where

import Myo.Output.Data.OutputParser (OutputParser)

data OutputHandler =
  OutputHandler {
    _parser :: OutputParser
    -- _filter :: OutputFilter a,
    -- _reporter :: OutputReporter a,
    -- _syntax :: OutputSyntax a
  }

deriving instance Show OutputHandler
