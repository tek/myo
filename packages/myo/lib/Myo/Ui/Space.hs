module Myo.Ui.Space where

import Myo.Ui.Data.Space (Space)
import qualified Myo.Ui.Data.UiState as UiState
import Myo.Ui.Data.UiState (UiState)

myoSpaces ::
  Member (AtomicState UiState) r =>
  Sem r [Space]
myoSpaces =
  atomicGets (.spaces)
