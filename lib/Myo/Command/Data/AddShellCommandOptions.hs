{-# LANGUAGE DeriveAnyClass #-}

module Myo.Command.Data.AddShellCommandOptions where

import Chiasma.Data.Ident (Ident)
import GHC.Generics (Generic)
import Ribosome.Msgpack.Decode (MsgpackDecode(..))

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Orphans ()

data AddShellCommandOptions =
  AddShellCommandOptions {
    ident :: Ident,
    lines :: [String],
    runner :: Maybe Ident,
    target :: Ident,
    lang :: Maybe CommandLanguage
  }
  deriving (Generic, MsgpackDecode)
