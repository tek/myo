module Myo.Output.Data.OutputParser where

import Data.Text (Text)
import qualified Text.Show

import Myo.Output.Data.OutputError (OutputError)
import Myo.Output.Data.ParsedOutput (ParsedOutput)

newtype OutputParser =
  OutputParser (Text -> Either OutputError ParsedOutput)

instance Text.Show.Show OutputParser where
 show _ = "OutputParser"
