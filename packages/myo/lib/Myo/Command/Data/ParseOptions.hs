module Myo.Command.Data.ParseOptions where

import Chiasma.Data.Ident (Ident)
import Ribosome (MsgpackDecode)

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Orphans ()

data ParseOptions =
  ParseOptions {
    pane :: Maybe Ident,
    command :: Maybe Ident,
    lang :: Maybe CommandLanguage
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, Default)
