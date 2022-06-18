module Myo.Command.Data.CommandLog where

data CommandLog =
  CommandLog {
    previous :: [ByteString],
    current :: ByteString
  }
  deriving stock (Eq, Show)
