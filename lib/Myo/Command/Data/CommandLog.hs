
module Myo.Command.Data.CommandLog where

import Data.ByteString (ByteString)
import Data.DeepLenses (deepLenses)

data CommandLog =
  CommandLog {
    _previous :: [ByteString],
     _current :: ByteString
  }
  deriving (Eq, Show)

deepLenses ''CommandLog
