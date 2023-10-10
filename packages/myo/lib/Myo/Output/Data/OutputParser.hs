module Myo.Output.Data.OutputParser where

import Myo.Output.Data.OutputError (OutputError)
import Myo.Output.Data.ParsedOutput (ParsedOutput)

newtype OutputParser r =
  OutputParser (Text -> Sem (Stop OutputError : r) ParsedOutput)

runOutputParser :: OutputParser r -> Text -> Sem (Stop OutputError : r) ParsedOutput
runOutputParser (OutputParser parser) =
  parser
