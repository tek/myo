module Myo.Command.Data.AddSystemCommandOptions where

import Chiasma.Data.Ident (Ident)
import Ribosome (MsgpackDecode, MsgpackEncode)

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Orphans ()

data AddSystemCommandOptions =
  AddSystemCommandOptions {
    ident :: Ident,
    lines :: [Text],
    runner :: Maybe Ident,
    target :: Maybe Ident,
    lang :: Maybe CommandLanguage,
    displayName :: Maybe Text,
    skipHistory :: Maybe Bool,
    kill :: Maybe Bool,
    capture :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)

cons :: Ident -> [Text] -> AddSystemCommandOptions
cons i l =
  AddSystemCommandOptions i l Nothing Nothing Nothing Nothing Nothing Nothing Nothing
