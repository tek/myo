module Myo.Command.Data.UiTarget where

import Chiasma.Data.Ident (Ident (Str, Uuid), Identifiable (identify), identText, parseIdent)
import qualified Data.UUID as UUID
import Prettyprinter (Pretty (pretty))
import Ribosome.Host.Class.Msgpack.Decode (MsgpackDecode (fromMsgpack))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))

newtype UiTarget =
  UiTarget { unUiTarget :: Ident }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

instance Pretty UiTarget where
  pretty =
    pretty . unUiTarget

instance Identifiable UiTarget where
  identify =
    unUiTarget

instance MsgpackDecode UiTarget where
  fromMsgpack =
    fmap (UiTarget . parseIdent) . fromMsgpack

instance MsgpackEncode UiTarget where
  toMsgpack (UiTarget (Str s)) =
    toMsgpack s
  toMsgpack (UiTarget (Uuid u)) =
    toMsgpack (UUID.toString u)

json ''UiTarget

uiTargetText :: UiTarget -> Text
uiTargetText =
  identText . unUiTarget
