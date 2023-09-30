module Myo.Command.Data.CommandSpec where

import Data.Aeson (FromJSON (parseJSON))
import GHC.Generics (Generically (Generically))
import Ribosome (MsgpackDecode (fromMsgpack), MsgpackEncode)
import Ribosome.Msgpack (decodeError, fromMsgpackGeneric)

import Myo.Command.Data.CommandTemplate (CommandTemplate, parseCommandTemplate, parseCommandTemplate', renderTemplate)
import Myo.Command.Data.Param (ParamDefaults)

data CommandSpec =
  CommandSpec {
    template :: CommandTemplate,
    params :: ParamDefaults
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode, ToJSON)

parseCommandSpec :: Either Text [Text] -> Either Text CommandSpec
parseCommandSpec raw = do
  tpl <- parseCommandTemplate raw
  pure (CommandSpec tpl mempty)

instance MsgpackDecode CommandSpec where
  fromMsgpack obj =
    case fromMsgpack obj of
      Right lns -> leftA decodeError (parseCommandSpec lns)
      Left _ -> fromMsgpackGeneric obj

instance FromJSON CommandSpec where
  parseJSON obj =
    fromLines <|> full
    where
      fromLines = do
        lns <- (Right <$> parseJSON obj) <|> (Left <$> parseJSON obj)
        leftA (fail . toString) (parseCommandSpec lns)
      full = do
        Generically spec <- parseJSON obj
        pure spec

parseCommandSpec' :: [Text] -> CommandSpec
parseCommandSpec' raw =
  CommandSpec (parseCommandTemplate' raw) mempty

renderCommandSpec :: CommandSpec -> [Text]
renderCommandSpec spec = renderTemplate spec.template
