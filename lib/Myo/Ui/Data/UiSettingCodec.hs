{-# LANGUAGE DeriveAnyClass #-}

module Myo.Ui.Data.UiSettingCodec where

import GHC.Generics (Generic)
import Ribosome.Msgpack.Decode (MsgpackDecode(..))
import Ribosome.Msgpack.Encode (MsgpackEncode(..))

import Myo.Ui.Data.AddLayoutOptions (AddLayoutOptions)
import Myo.Ui.Data.AddPaneOptions (AddPaneOptions)

data UiSettingCodec =
  UiSettingCodec {
    layouts :: [AddLayoutOptions],
    panes :: [AddPaneOptions]
  }
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode)
