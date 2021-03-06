{-# LANGUAGE DeriveAnyClass #-}

module Myo.Command.Data.AddShellCommandOptions where


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
    skipHistory :: Maybe Bool,
    kill :: Maybe Bool,
    capture :: Maybe Bool
  }
  deriving (Eq, Show, Generic, MsgpackDecode, MsgpackEncode)
