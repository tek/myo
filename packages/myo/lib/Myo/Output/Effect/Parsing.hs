module Myo.Output.Effect.Parsing where

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Output.Data.ParsedOutput (ParsedOutput)

data Parsing :: Effect where
  Parse :: CommandLanguage -> Text -> Parsing m (Maybe (NonEmpty ParsedOutput))

makeSem ''Parsing
