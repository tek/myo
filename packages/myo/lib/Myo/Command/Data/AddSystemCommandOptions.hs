module Myo.Command.Data.AddSystemCommandOptions where

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Orphans ()

data AddSystemCommandOptions =
  AddSystemCommandOptions {
    ident :: Ident,
    lines :: [Text],
    runner :: Maybe Ident,
    target :: Maybe Ident,
    lang :: Maybe CommandLanguage,
    displayName :: Maybe Text,
    skipHistory :: Maybe Bool,
    kill :: Maybe Bool,
    capture :: Maybe Bool
  }
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode)
