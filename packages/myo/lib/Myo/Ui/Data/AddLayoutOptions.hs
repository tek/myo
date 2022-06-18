module Myo.Ui.Data.AddLayoutOptions where

import Chiasma.Data.Ident (Ident)
import Ribosome (MsgpackDecode, MsgpackEncode)

import Myo.Orphans ()

data AddLayoutOptions =
  AddLayoutOptions {
    layout :: Ident,
    vertical :: Maybe Bool,
    ident :: Maybe Ident,
    minimized :: Maybe Bool,
    minSize :: Maybe Float,
    maxSize :: Maybe Float,
    fixedSize :: Maybe Float,
    minimizedSize :: Maybe Float,
    weight :: Maybe Float,
    position :: Maybe Float
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default, MsgpackDecode, MsgpackEncode)
