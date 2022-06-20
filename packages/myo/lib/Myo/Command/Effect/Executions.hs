module Myo.Command.Effect.Executions where

import Chiasma.Data.Ident (Ident)
import Control.Lens (over)
import Data.Generics.Labels ()

import Myo.Command.Data.Execution (Execution, ExecutionState)

data Executions :: Effect where
  Update :: Ident -> (Execution -> Execution) -> Executions m ()

makeSem ''Executions

updateState ::
  Member Executions r =>
  Ident ->
  (ExecutionState -> ExecutionState) ->
  Sem r ()
updateState i =
  update i . over (#monitor . #state)
