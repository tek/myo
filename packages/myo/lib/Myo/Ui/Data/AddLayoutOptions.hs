module Myo.Ui.Data.AddLayoutOptions where

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
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode, Default)
