module Myo.Output.Lang.Nix.Data.NixEvent where

import Myo.Output.Data.Location (Location)

data NixEvent =
  NixEvent {
     location :: Location,
     message :: Text
  }
  deriving stock (Eq, Show)
