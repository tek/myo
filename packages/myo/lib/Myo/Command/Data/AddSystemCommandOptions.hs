module Myo.Command.Data.AddSystemCommandOptions where

import Chiasma.Data.Ident (Ident)
import Ribosome (MsgpackDecode, MsgpackEncode)

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Command.Data.CommandTemplate (CommandTemplate, parseCommandTemplate')
import Myo.Command.Data.Param (ParamDefaults)
import Myo.Command.Data.UiTarget (UiTarget)
import Myo.Data.CommandId (CommandId)
import Myo.Data.CommandName (CommandName)
import Myo.Orphans ()

data AddSystemCommandOptions =
  AddSystemCommandOptions {
    ident :: CommandId,
    lines :: CommandTemplate,
    params :: Maybe ParamDefaults,
    runner :: Maybe Ident,
    target :: Maybe UiTarget,
    lang :: Maybe CommandLanguage,
    displayName :: Maybe CommandName,
    skipHistory :: Maybe Bool,
    kill :: Maybe Bool,
    capture :: Maybe Bool,
    commandShell :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)

cons :: CommandId -> [Text] -> AddSystemCommandOptions
cons i l =
  AddSystemCommandOptions i (parseCommandTemplate' l)
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

systemOptions :: CommandId -> [Text] -> AddSystemCommandOptions
systemOptions = cons
