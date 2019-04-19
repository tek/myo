{-# LANGUAGE DeriveAnyClass #-}

module Myo.Command.Data.CommandSettingCodec where

import GHC.Generics (Generic)
import Ribosome.Msgpack.Decode (MsgpackDecode(..))
import Ribosome.Msgpack.Encode (MsgpackEncode(..))

import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions)

data CommandSettingCodec =
  CommandSettingCodec {
    system :: Maybe [AddSystemCommandOptions],
    shell :: Maybe [AddShellCommandOptions]
  }
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode)
