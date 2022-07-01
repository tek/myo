module Myo.Command.Interpreter.Executor.Generic where

import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask)
import qualified Myo.Command.Effect.Executor as Executor
import Myo.Command.Effect.Executor (Executor (CaptureOutput, Execute))

interceptExecutor ::
  Member (Executor !! err) r =>
  (RunTask -> Sem (Stop err : r) (Maybe t)) ->
  (t -> Sem (Stop err : r) ()) ->
  (t -> Sem (Stop err : r) ()) ->
  Sem r a ->
  Sem r a
interceptExecutor accept exec capture =
  interceptResumable \case
    Execute task ->
      accept task >>= \case
        Just t -> exec t
        Nothing -> restop (Executor.execute task)
    CaptureOutput task ->
      accept task >>= \case
        Just t -> capture t
        Nothing -> restop (Executor.captureOutput task)

interpretExecutorFailWith ::
  (RunTask -> err) ->
  InterpreterFor (Executor !! err) r
interpretExecutorFailWith err =
  interpretResumable \case
    Execute task ->
      stop (err task)
    CaptureOutput task ->
      stop (err task)

interpretExecutorFail :: InterpreterFor (Executor !! RunError) r
interpretExecutorFail =
  interpretExecutorFailWith RunError.NoRunner

captureUnsupported ::
  Member (Stop RunError) r =>
  Text ->
  t ->
  Sem r ()
captureUnsupported name _ =
  stop (RunError.Unsupported name "capture")
