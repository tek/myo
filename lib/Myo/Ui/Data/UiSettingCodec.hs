{-# LANGUAGE DeriveAnyClass #-}

module Myo.Ui.Data.UiSettingCodec where

import Myo.Ui.Data.AddLayoutOptions (AddLayoutOptions)
import Myo.Ui.Data.AddPaneOptions (AddPaneOptions)

data UiSettingCodec =
  UiSettingCodec {
    layouts :: Maybe [AddLayoutOptions],
    panes :: Maybe [AddPaneOptions]
  }
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode)
