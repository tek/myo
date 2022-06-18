module Myo.Ui.Data.UiSettingCodec where

import Ribosome (MsgpackDecode, MsgpackEncode)

import Myo.Ui.Data.AddLayoutOptions (AddLayoutOptions)
import Myo.Ui.Data.AddPaneOptions (AddPaneOptions)

data UiSettingCodec =
  UiSettingCodec {
    layouts :: Maybe [AddLayoutOptions],
    panes :: Maybe [AddPaneOptions]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)
