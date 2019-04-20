{-# LANGUAGE DeriveAnyClass #-}

module Myo.Ui.Data.AddLayoutOptions where

import Chiasma.Data.Ident (Ident)
import Data.Default (Default)
import GHC.Generics (Generic)
import Ribosome.Msgpack.Decode (MsgpackDecode(..))
import Ribosome.Msgpack.Encode (MsgpackEncode(..))

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
