{-# LANGUAGE DeriveAnyClass #-}

module Myo.Command.Data.RunLineOptions where

import Myo.Command.Data.Command (CommandLanguage)

data RunLineOptions =
  RunLineOptions {
    line :: Maybe Text,
    lines :: Maybe [Text],
    target :: Maybe Ident,
    runner :: Maybe Ident,
    lang :: Maybe CommandLanguage,
    skipHistory :: Maybe Bool,
    kill :: Maybe Bool
  }
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode)
