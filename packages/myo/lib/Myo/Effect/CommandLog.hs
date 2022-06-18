module Myo.Effect.CommandLog where

import Chiasma.Data.Ident (Ident)

data CommandLog :: Effect where
  Start :: Ident -> CommandLog m ()

makeSem ''CommandLog
