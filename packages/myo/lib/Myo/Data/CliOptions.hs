module Myo.Data.CliOptions where

import Myo.Command.Data.SocatExe (SocatExe)

data CliOptions =
  CliOptions {
    socat :: Maybe SocatExe
  }
  deriving stock (Eq, Show, Generic)
