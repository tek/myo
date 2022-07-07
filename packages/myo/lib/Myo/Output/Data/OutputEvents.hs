module Myo.Output.Data.OutputEvents where

import Data.Vector (Vector)

import Myo.Output.Data.OutputEvent (OutputEvent)

newtype OutputEvents =
  OutputEvents {
    events :: Vector OutputEvent
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)
