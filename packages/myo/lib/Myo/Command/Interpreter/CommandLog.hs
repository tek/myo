module Myo.Command.Interpreter.CommandLog where

import Chiasma.Data.Ident (Ident)
import Conc (interpretAtomic)
import qualified Data.Map.Strict as Map
import Data.Sequence ((|>))

import Myo.Command.Data.CommandOutput (CommandOutput (CommandOutput))
import Myo.Effect.CommandLog (CommandLog (Append, Start, Get))

append ::
  Text ->
  Maybe CommandOutput ->
  CommandOutput
append chunk = \case
  Just (CommandOutput h c) ->
    CommandOutput h (c |> chunk)
  Nothing ->
    CommandOutput mempty (pure chunk)

currentOutput :: CommandOutput -> Text
currentOutput (CommandOutput _ current) =
  mconcat (toList current)

interpretCommandLog ::
  Member (Embed IO) r =>
  InterpreterFor CommandLog r
interpretCommandLog =
  interpretAtomic (mempty :: Map Ident CommandOutput) .
  reinterpret \case
    Start _ ->
      undefined
    Append ident chunk ->
      atomicModify' \ s ->
        Map.alter (Just . append chunk) ident s
    Get ident ->
      fmap currentOutput <$> atomicGets (Map.lookup ident)
