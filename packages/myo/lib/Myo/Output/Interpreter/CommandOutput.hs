module Myo.Output.Interpreter.CommandOutput where

import qualified Data.Map.Strict as Map

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Output.Data.OutputError (OutputError (NoHandler))
import Myo.Output.Data.ParsedOutput (ParsedOutput)
import Myo.Output.Effect.CommandOutput (CommandOutput (Parse))

interpretCommandOutput ::
  Map CommandLanguage (Text -> Sem (Stop OutputError : r) ParsedOutput) ->
  InterpreterFor (CommandOutput !! OutputError) r
interpretCommandOutput handlers =
  interpretResumable \case
    Parse lang out -> do
      parser <- stopNote (NoHandler lang) (Map.lookup lang handlers)
      parser out
