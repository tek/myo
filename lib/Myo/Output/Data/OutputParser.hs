module Myo.Output.Data.OutputParser where

import Data.Text (Text)

import Myo.Output.Data.OutputError (OutputError)
import Myo.Output.Data.ParsedOutput (ParsedOutput)

newtype OutputParser =
  OutputParser (Text -> Either OutputError ParsedOutput)

instance Show OutputParser where
 show _ = "OutputParser"
