module Myo.Command.Data.AddShellCommandOptions where

import Chiasma.Data.Ident (Ident)
import Ribosome (MsgpackDecode, MsgpackEncode)

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Command.Data.CommandTemplate (CommandTemplate, parseCommandTemplate')
import Myo.Command.Data.Param (ParamDefaults)
import Myo.Data.CommandId (CommandId)
import Myo.Orphans ()

data AddShellCommandOptions =
  AddShellCommandOptions {
    ident :: CommandId,
    lines :: CommandTemplate,
    params :: Maybe ParamDefaults,
    runner :: Maybe Ident,
    target :: CommandId,
    lang :: Maybe CommandLanguage,
    displayName :: Maybe Text,
    skipHistory :: Maybe Bool,
    kill :: Maybe Bool,
    capture :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)

cons :: CommandId -> [Text] -> CommandId -> AddShellCommandOptions
cons i l t =
  AddShellCommandOptions i (parseCommandTemplate' l) Nothing Nothing t Nothing Nothing Nothing Nothing Nothing

shellOptions :: CommandId -> [Text] -> CommandId -> AddShellCommandOptions
shellOptions = cons
