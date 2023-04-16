module Myo.Interpreter.Commands where

import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandState as CommandState
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.HistoryEntry as HistoryEntry
import Myo.Effect.Commands (Commands (Latest))

interpretCommands ::
  Member (AtomicState CommandState) r =>
  InterpreterFor (Commands !! CommandError) r
interpretCommands =
  interpretResumable \case
    Latest ->
      fmap (.command) . stopNote CommandError.NoCommands . head =<< atomicGets (.history)
