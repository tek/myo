module Myo.Command.Interpreter.Executions where

import Chiasma.Data.Ident (Ident)
import Conc (interpretAtomic)

import Myo.Command.Data.Execution (Execution)
import Myo.Command.Effect.Executions (Executions)
import qualified Myo.Command.Effect.Executions as Executions

interpretExecutions ::
  Member (Embed IO) r =>
  InterpreterFor Executions r
interpretExecutions =
  interpretAtomic mempty .
  reinterpret \case
    Executions.Set _ ->
      undefined
