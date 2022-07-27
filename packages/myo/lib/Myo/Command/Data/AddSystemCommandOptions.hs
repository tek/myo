module Myo.Command.Data.AddSystemCommandOptions where

import Chiasma.Data.Ident (Ident)
import Ribosome (MsgpackDecode, MsgpackEncode)

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Command.Data.UiTarget (UiTarget)
import Myo.Data.CommandId (CommandId)
import Myo.Orphans ()

data AddSystemCommandOptions =
  AddSystemCommandOptions {
    ident :: CommandId,
    lines :: [Text],
    runner :: Maybe Ident,
    target :: Maybe UiTarget,
    lang :: Maybe CommandLanguage,
    displayName :: Maybe Text,
    skipHistory :: Maybe Bool,
    kill :: Maybe Bool,
    capture :: Maybe Bool,
    commandShell :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)

cons :: CommandId -> [Text] -> AddSystemCommandOptions
cons i l =
  AddSystemCommandOptions i l Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
