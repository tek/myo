{-# OPTIONS_GHC -fno-warn-orphans #-}

module Myo.Orphans where

import Chiasma.Data.Ident (Ident (..), parseIdent)
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry)
import qualified Data.UUID as UUID (toString)
import Ribosome (MsgpackDecode (fromMsgpack), MsgpackEncode (toMsgpack))

instance MsgpackDecode Ident where
  fromMsgpack =
    fmap parseIdent . fromMsgpack

instance MsgpackEncode Ident where
  toMsgpack (Str s) =
    toMsgpack s
  toMsgpack (Uuid u) =
    toMsgpack (UUID.toString u)

deriving anyclass instance MsgpackDecode ViewGeometry
