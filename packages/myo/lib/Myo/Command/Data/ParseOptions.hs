module Myo.Command.Data.ParseOptions where

import Chiasma.Data.Ident (Ident)
import Ribosome (MsgpackDecode)

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Data.CommandId (CommandId)
import Myo.Orphans ()

data ParseOptions =
  ParseOptions {
    pane :: Maybe Ident,
    command :: Maybe CommandId,
    lang :: Maybe CommandLanguage
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, Default)
