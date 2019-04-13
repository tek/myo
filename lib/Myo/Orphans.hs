{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}

module Myo.Orphans where

import Chiasma.Data.Ident (Ident(..))
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry)
import Data.ByteString.Internal (unpackChars)
import Data.MessagePack (Object(ObjectString))
import qualified Data.UUID as UUID (toString)
import Ribosome.Msgpack.Decode (MsgpackDecode(..))
import Ribosome.Msgpack.Encode (MsgpackEncode(..))
import qualified Ribosome.Msgpack.Util as Util (illegalType, string)

instance MsgpackDecode Ident where
  fromMsgpack (ObjectString s) = Right $ Str $ unpackChars s
  fromMsgpack a = Util.illegalType "Ident" a

instance MsgpackEncode Ident where
  toMsgpack (Str s) = toMsgpack s
  toMsgpack (Uuid u) = Util.string (UUID.toString u)

deriving instance MsgpackDecode ViewGeometry
