module Myo.Output.Lang.Nix.Data.NixEvent where

import Myo.Output.Data.Location (Location)

data NixEvent =
  NixEvent {
     _location :: Location,
     _message :: Text
  }
  deriving stock (Eq, Show)
