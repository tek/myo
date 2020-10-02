{-# LANGUAGE DeriveAnyClass #-}

module Myo.Command.Data.ParseOptions where


import Myo.Command.Data.Command (CommandLanguage)
import Myo.Orphans ()

data ParseOptions =
  ParseOptions {
    pane :: Maybe Ident,
    command :: Maybe Ident,
    lang :: Maybe CommandLanguage
  }
  deriving (Eq, Show, Generic, MsgpackDecode)
