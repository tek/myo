module Myo.Command.Data.RunEvent where

import Chiasma.Data.Ident (Ident)

data RunEventDetails =
  Output Text
  |
  Terminated
  deriving stock (Eq, Show)

data RunEvent =
  RunEvent {
    command :: Ident,
    details :: RunEventDetails
  }
  deriving stock (Eq, Show)
