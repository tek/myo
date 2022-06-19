module Myo.Command.Interpreter.Executor.Null where

import qualified Myo.Effect.Executor as Executor
import Myo.Effect.Executor (Executor)

interpretExecutorNull ::
  InterpreterFor (Executor task !! err) r
interpretExecutorNull =
  interpretResumable \case
    Executor.Accept _ -> pure Nothing
    Executor.Run _ -> pure Nothing
