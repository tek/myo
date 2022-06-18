module Myo.Command.Data.VimTestPosition where

import Ribosome (MsgpackDecode, MsgpackEncode)

data VimTestPosition =
  VimTestPosition {
    file :: Text,
    line :: Int,
    col :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)
