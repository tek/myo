module Myo.Output.Interpreter.Parsing where

import qualified Data.Map.Strict as Map

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Output.Data.OutputError (OutputError (NoHandler))
import qualified Myo.Output.Data.ParsedOutput as ParsedOutput
import Myo.Output.Data.ParsedOutput (ParsedOutput)
import Myo.Output.Effect.Parsing (Parsing (Parse))

type OutputParser r =
  Text -> Sem (Stop OutputError : r) ParsedOutput

interpretParsing ::
  Map CommandLanguage [OutputParser r] ->
  InterpreterFor (Parsing !! OutputError) r
interpretParsing parsers =
  interpretResumable \case
    Parse lang out -> do
      langParsers <- stopNote (NoHandler lang) (Map.lookup lang parsers)
      nonEmpty . filter (not . ParsedOutput.empty) <$> traverse ($ out) langParsers
