module Myo.Command.Data.RunLineOptions where

import Chiasma.Data.Ident (Ident)
import Ribosome (MsgpackDecode, MsgpackEncode)

import Myo.Command.Data.Command (CommandLanguage)

data RunLineOptions =
  RunLineOptions {
    line :: Maybe Text,
    lines :: Maybe [Text],
    target :: Maybe Ident,
    runner :: Maybe Ident,
    lang :: Maybe CommandLanguage,
    skipHistory :: Maybe Bool,
    kill :: Maybe Bool,
    capture :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode, Default)
