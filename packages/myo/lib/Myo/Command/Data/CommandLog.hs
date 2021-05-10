module Myo.Command.Data.CommandLog where

data CommandLog =
  CommandLog {
    _previous :: [ByteString],
     _current :: ByteString
  }
  deriving (Eq, Show)

deepLenses ''CommandLog
