module Myo.Data.CommandId where

import Chiasma.Data.Ident (Ident (Str, Uuid), identText, parseIdent)
import qualified Data.UUID as UUID
import Prettyprinter (Pretty (pretty))
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))

newtype CommandId =
  CommandId { unCommandId :: Ident }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

instance Pretty CommandId where
  pretty =
    pretty . (.unCommandId)

instance MsgpackDecode CommandId where
  fromMsgpack =
    fmap (CommandId . parseIdent) . fromMsgpack

instance MsgpackEncode CommandId where
  toMsgpack (CommandId (Str s)) =
    toMsgpack s
  toMsgpack (CommandId (Uuid u)) =
    toMsgpack (UUID.toString u)

json ''CommandId

commandIdText :: CommandId -> Text
commandIdText =
  identText . (.unCommandId)
