module Myo.Output.Data.OutputParser where

import qualified Text.Show

import Myo.Output.Data.OutputError (OutputError)
import Myo.Output.Data.ParsedOutput (ParsedOutput)

data OutputParser =
  OutputParser {
    parse :: Text -> Either OutputError ParsedOutput,
    sanitize :: ParsedOutput -> IO ParsedOutput
  }

instance Show OutputParser where
 show _ =
   "OutputParser"
