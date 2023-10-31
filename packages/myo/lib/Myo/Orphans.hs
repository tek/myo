{-# options_ghc -fno-warn-orphans #-}

module Myo.Orphans where

import Chiasma.Data.Axis (Axis (Horizontal, Vertical))
import Chiasma.Data.Ident (Ident (..), parseIdent)
import Chiasma.Ui.Data.View (Layout, LayoutView, PaneView, Pane)
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry)
import Chiasma.Ui.Data.ViewState (ViewState)
import qualified Data.UUID as UUID (toString)
import Exon (exon)
import Ribosome (MsgpackDecode (fromMsgpack), MsgpackEncode (toMsgpack))
import Ribosome.Msgpack (decodeError)

instance MsgpackDecode Ident where
  fromMsgpack =
    fmap parseIdent . fromMsgpack

instance MsgpackEncode Ident where
  toMsgpack (Str s) =
    toMsgpack s
  toMsgpack (Uuid u) =
    toMsgpack (UUID.toString u)

instance MsgpackDecode Axis where
  fromMsgpack =
    fromMsgpack >=> \case
      "horizontal" -> Right Horizontal
      "vertical" -> Right Vertical
      a -> decodeError [exon|Invalid value for Axis: #{a}|]

deriving anyclass instance MsgpackDecode ViewGeometry
deriving anyclass instance MsgpackDecode ViewState
deriving anyclass instance MsgpackDecode Layout
deriving anyclass instance MsgpackDecode LayoutView
deriving anyclass instance MsgpackDecode Pane
deriving anyclass instance MsgpackDecode PaneView

instance MsgpackEncode Axis where
  toMsgpack =
    toMsgpack @Text . \case
      Horizontal -> "horizontal"
      Vertical -> "vertical"

deriving anyclass instance MsgpackEncode ViewGeometry
deriving anyclass instance MsgpackEncode ViewState
deriving anyclass instance MsgpackEncode Layout
deriving anyclass instance MsgpackEncode LayoutView
deriving anyclass instance MsgpackEncode Pane
deriving anyclass instance MsgpackEncode PaneView
