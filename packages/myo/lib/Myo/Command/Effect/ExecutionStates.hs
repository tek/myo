module Myo.Command.Effect.ExecutionStates where

import Myo.Command.Data.Execution (ExecutionState)

data ExecutionStates :: Effect where
  Set :: ExecutionState -> ExecutionStates m ()

makeSem ''ExecutionStates
