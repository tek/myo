module Myo.Command.Data.RunLineOptions where

import Chiasma.Data.Ident (Ident)
import Ribosome (MsgpackDecode, MsgpackEncode)

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Command.Data.CommandSpec (CommandSpec)

data RunLineOptions =
  RunLineOptions {
    line :: Maybe CommandSpec,
    lines :: Maybe CommandSpec,
    target :: Maybe Ident,
    runner :: Maybe Ident,
    lang :: Maybe CommandLanguage,
    skipHistory :: Maybe Bool,
    kill :: Maybe Bool,
    capture :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode, Default)
