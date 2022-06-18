module Myo.Command.Data.CommandSettingCodec where

import Ribosome (MsgpackDecode, MsgpackEncode)

import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions)

data CommandSettingCodec =
  CommandSettingCodec {
    system :: Maybe [AddSystemCommandOptions],
    shell :: Maybe [AddShellCommandOptions]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)
