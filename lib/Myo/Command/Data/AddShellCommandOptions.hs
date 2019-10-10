{-# LANGUAGE DeriveAnyClass #-}

module Myo.Command.Data.AddShellCommandOptions where

import Chiasma.Data.Ident (Ident)
import GHC.Generics (Generic)
import Ribosome.Msgpack.Decode (MsgpackDecode(..))
import Ribosome.Msgpack.Encode (MsgpackEncode(..))

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Orphans ()

data AddShellCommandOptions =
  AddShellCommandOptions {
    ident :: Ident,
    lines :: [Text],
    runner :: Maybe Ident,
    target :: Ident,
    lang :: Maybe CommandLanguage,
    displayName :: Maybe Text,
    skipHistory :: Maybe Bool
  }
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode)
