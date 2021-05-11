module Myo.Ui.Data.AddPaneOptions where

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
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode, Default)
