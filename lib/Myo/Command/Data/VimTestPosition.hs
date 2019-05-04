{-# LANGUAGE DeriveAnyClass #-}

module Myo.Command.Data.VimTestPosition where

import GHC.Generics (Generic)
import Ribosome.Msgpack.Decode (MsgpackDecode)
import Ribosome.Msgpack.Encode (MsgpackEncode)

data VimTestPosition =
  VimTestPosition {
    file :: Text,
    line :: Int,
    col :: Int
  }
  deriving (Eq, Show, Generic, MsgpackEncode, MsgpackDecode)
