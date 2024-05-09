module Myo.Command.Data.Param where

import Data.Aeson (FromJSON (parseJSON), FromJSONKey, ToJSON (toJSON), ToJSONKey)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Map (DMap)
import Data.GADT.Compare.TH (DeriveGCompare (deriveGCompare), DeriveGEQ (deriveGEq))
import Data.GADT.Show.TH (deriveGShow)
import qualified Data.Text as Text
import Ribosome (MsgpackDecode, MsgpackEncode)
import Ribosome.Msgpack (MsgpackDecode (fromMsgpack), MsgpackEncode (toMsgpack))

newtype ParamId =
  ParamId Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, MsgpackEncode, MsgpackDecode, ToJSONKey, FromJSONKey)

json ''ParamId

data ParamValue =
  ParamValue Text
  |
  ParamFlag Bool
  deriving stock (Eq, Show, Generic)

instance IsString ParamValue where
  fromString = ParamValue . fromString

instance MsgpackEncode ParamValue where
  toMsgpack = \case
    ParamValue t -> toMsgpack t
    ParamFlag f -> toMsgpack f

instance MsgpackDecode ParamValue where
  fromMsgpack =
    fmap (either ParamValue ParamFlag) . fromMsgpack

instance ToJSON ParamValue where
  toJSON = \case
    ParamValue t -> toJSON t
    ParamFlag f -> toJSON f

instance FromJSON ParamValue where
  parseJSON v =
    (ParamValue <$> parseJSON v) <|> (ParamFlag <$> parseJSON v)

type ParamValues = Map ParamId ParamValue

parseParamFlag :: Text -> Either Text ParamValue
parseParamFlag =
  Text.toLower >>> \case
    "true" -> Right (ParamFlag True)
    "false" -> Right (ParamFlag False)
    _ -> Left "A boolean flag can only be 'true' or 'false'"

renderParamValue :: ParamValue -> Text
renderParamValue = \case
  ParamValue t -> t
  ParamFlag f -> show f

newtype ParamDefault =
  ParamDefault ParamValue
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, MsgpackEncode, MsgpackDecode)

json ''ParamDefault

type ParamDefaults = Map ParamId ParamDefault

renderParamDefault :: ParamDefault -> Text
renderParamDefault =
  renderParamValue . coerce

type ParamTag :: Type -> Type
data ParamTag a where
  ParamText :: Text -> ParamTag Text
  ParamBool :: Text -> ParamTag Bool

deriveGEq ''ParamTag
deriveGShow ''ParamTag
deriveGCompare ''ParamTag
deriveArgDict ''ParamTag

instance IsString (ParamTag Text) where
  fromString = ParamText . fromString

instance IsString (ParamTag Bool) where
  fromString = ParamBool . fromString

paramTagName :: ParamTag a -> Text
paramTagName = \case
  ParamText n -> n
  ParamBool n -> n

paramTagId :: ParamTag a -> ParamId
paramTagId =
  ParamId . paramTagName

paramTagType :: ParamTag a -> Text
paramTagType = \case
  ParamText _ -> "string"
  ParamBool _ -> "boolean"

type OtherTag :: Type -> Type
type family OtherTag a where
  OtherTag Text = Bool
  OtherTag Bool = Text

otherParamTag :: ParamTag a -> ParamTag (OtherTag a)
otherParamTag = \case
  ParamText n -> ParamBool n
  ParamBool n -> ParamText n

data DefinedParam a =
  DefinedParam a
  |
  UndefinedParam
  deriving stock (Eq, Show, Generic)

type DefinedParams = DMap ParamTag DefinedParam

data ParamEnv =
  ParamEnv {
    defaults :: DefinedParams,
    overrides :: DefinedParams,
    cli :: DefinedParams,
    resolved :: DefinedParams
  }
  deriving stock (Eq, Show, Generic)
