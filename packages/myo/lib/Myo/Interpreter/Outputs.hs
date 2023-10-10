module Myo.Interpreter.Outputs where

import Conc (interpretAtomic)

import Myo.Command.Data.OutputState (OutputState)
import Myo.Effect.Outputs (Outputs (..))

overOutput ::
  Member (AtomicState (Maybe OutputState)) r =>
  (OutputState -> OutputState) ->
  Sem r ()
overOutput f =
  atomicModify' (fmap f)

interpretOutputs ::
  Member (Embed IO) r =>
  InterpreterFor Outputs r
interpretOutputs =
  interpretAtomic (Nothing :: Maybe OutputState) .
  reinterpret \case
    CurrentOutput ->
      atomicGet
    SetCurrentOutput o ->
      atomicPut (Just o)
    SetCurrentReport report ->
      overOutput (#report ?~ report)
    SetCurrentEvent e ->
      atomicModify' (fmap (#currentEvent .~ e))
