module Myo.Command.Data.TmuxMonitorError where

import Chiasma.Data.CodecError (CodecError)
import Log (Severity (Error))
import Ribosome (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))

data TmuxMonitorError sre =
  SocketReader sre
  |
  TmuxCodec CodecError
  deriving stock (Eq, Show)

instance (
    ToErrorMessage sre
  ) => ToErrorMessage (TmuxMonitorError sre) where
    toErrorMessage = \case
      SocketReader sre ->
        toErrorMessage sre
      TmuxCodec err ->
        ErrorMessage "tmux codec error" ["TmuxMonitorError.TmuxCodec:", show err] Error
