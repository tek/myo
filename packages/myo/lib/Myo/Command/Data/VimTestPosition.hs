module Myo.Command.Data.VimTestPosition where

data VimTestPosition =
  VimTestPosition {
    file :: Text,
    line :: Int,
    col :: Int
  }
  deriving (Eq, Show, Generic, MsgpackEncode, MsgpackDecode)
