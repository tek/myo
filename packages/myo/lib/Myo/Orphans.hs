{-# OPTIONS_GHC -fno-warn-orphans #-}

module Myo.Orphans where

import Chiasma.Data.Ident (Ident(..))
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry)
import Data.MessagePack (Object(ObjectString))
import qualified Data.UUID as UUID (toString)
import qualified Ribosome.Msgpack.Util as Util (illegalType, string)

instance MsgpackDecode Ident where
  fromMsgpack (ObjectString s) = Right . Str . decodeUtf8 $ s
  fromMsgpack a = Util.illegalType "Ident" a

instance MsgpackEncode Ident where
  toMsgpack (Str s) = toMsgpack s
  toMsgpack (Uuid u) = Util.string (UUID.toString u)

deriving instance MsgpackDecode ViewGeometry
