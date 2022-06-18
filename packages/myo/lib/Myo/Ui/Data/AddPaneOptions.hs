module Myo.Ui.Data.AddPaneOptions where

import Chiasma.Data.Ident (Ident)
import Ribosome (MsgpackDecode, MsgpackEncode)

import Myo.Orphans ()

data AddPaneOptions =
  AddPaneOptions {
    layout :: Ident,
    ident :: Maybe Ident,
    space :: Maybe Ident,
    window :: Maybe Ident,
    minimized :: Maybe Bool,
    minSize :: Maybe Float,
    maxSize :: Maybe Float,
    fixedSize :: Maybe Float,
    minimizedSize :: Maybe Float,
    weight :: Maybe Float,
    position :: Maybe Float,
    pin :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default, MsgpackDecode, MsgpackEncode)
