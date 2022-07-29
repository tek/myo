module Myo.Command.Data.TmuxMonitorError where

import Ribosome (Reportable (toReport))

import Myo.Data.ViewError (ViewError)

data TmuxMonitorError sre =
  SocketReader sre
  |
  View ViewError
  deriving stock (Eq, Show)

instance (
    Reportable sre
  ) => Reportable (TmuxMonitorError sre) where
    toReport = \case
      SocketReader sre ->
        toReport sre
      View err ->
        toReport err
