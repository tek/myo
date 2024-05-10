module Myo.Command.Interpreter.Backend.Generic where

import Conc (interpretAtomic)

import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask)
import qualified Myo.Command.Effect.Backend as Backend
import Myo.Command.Effect.Backend (Backend (CaptureOutput, Execute, Render))

interceptBackend ::
  Member (Backend !! err) r =>
  (RunTask -> Sem (Stop err : r) (Maybe t)) ->
  (t -> Sem (Stop err : r) ()) ->
  (t -> Sem (Stop err : r) ()) ->
  Sem (Stop err : r) () ->
  Sem r a ->
  Sem r a
interceptBackend accept exec capture render =
  interceptResumable \case
    Execute task ->
      accept task >>= \case
        Just t -> exec t
        Nothing -> restop (Backend.execute task)
    CaptureOutput task ->
      accept task >>= \case
        Just t -> capture t
        Nothing -> restop (Backend.captureOutput task)
    Render ->
      render *> restop Backend.render

interpretBackendFailWith ::
  (RunTask -> err) ->
  InterpreterFor (Backend !! err) r
interpretBackendFailWith err =
  interpretResumable \case
    Execute task ->
      stop (err task)
    CaptureOutput task ->
      stop (err task)
    Render ->
      unit

interpretBackendFail :: InterpreterFor (Backend !! RunError) r
interpretBackendFail =
  interpretBackendFailWith RunError.NoRunner

captureUnsupported ::
  Member (Stop RunError) r =>
  Text ->
  t ->
  Sem r ()
captureUnsupported name _ =
  stop (RunError.Unsupported name "capture")

interpretBackendTrace ::
  Members [Backend !! RunError, AtomicState [RunTask]] r =>
  Sem r a ->
  Sem r a
interpretBackendTrace =
  interceptBackend (pure . Just) (atomicModify' . (:)) (captureUnsupported "trace") unit

withBackendTrace ::
  Members [Backend !! RunError, Embed IO] r =>
  InterpreterFor (AtomicState [RunTask]) r
withBackendTrace =
  interpretAtomic [] .
  interpretBackendTrace
