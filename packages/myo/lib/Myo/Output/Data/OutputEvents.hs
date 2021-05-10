module Myo.Output.Data.OutputEvents where

import Data.Vector (Vector)
import Myo.Output.Data.OutputEvent (OutputEvent)

newtype OutputEvents =
  OutputEvents {
    _events :: Vector OutputEvent
  }
  deriving (Eq, Show)
  deriving newtype (Semigroup, Monoid)

makeClassy ''OutputEvents
