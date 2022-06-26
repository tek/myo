module Myo.Output.Effect.CommandOutput where

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Output.Data.ParsedOutput (ParsedOutput)

data CommandOutput :: Effect where
  Parse :: CommandLanguage -> Text -> CommandOutput m ParsedOutput

makeSem ''CommandOutput
