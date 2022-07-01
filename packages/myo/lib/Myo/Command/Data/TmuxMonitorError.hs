module Myo.Command.Data.TmuxMonitorError where

import Ribosome (ToErrorMessage (toErrorMessage))

import Myo.Data.ViewError (ViewError)

data TmuxMonitorError sre =
  SocketReader sre
  |
  View ViewError
  deriving stock (Eq, Show)

instance (
    ToErrorMessage sre
  ) => ToErrorMessage (TmuxMonitorError sre) where
    toErrorMessage = \case
      SocketReader sre ->
        toErrorMessage sre
      View err ->
        toErrorMessage err
