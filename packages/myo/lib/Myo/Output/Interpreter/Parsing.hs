module Myo.Output.Interpreter.Parsing where

import qualified Data.Map.Strict as Map

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Output.Data.OutputError (OutputError (NoHandler))
import qualified Myo.Output.Data.ParsedOutput as ParsedOutput
import Myo.Output.Effect.Parsing (OutputParser, Parsing (Parse))

interpretParsing ::
  Map CommandLanguage [OutputParser r] ->
  InterpreterFor (Parsing !! OutputError) r
interpretParsing parsers =
  interpretResumable \case
    Parse lang out -> do
      langParsers <- stopNote (NoHandler lang) (Map.lookup lang parsers)
      nonEmpty . filter (not . ParsedOutput.empty) <$> traverse ($ out) langParsers
