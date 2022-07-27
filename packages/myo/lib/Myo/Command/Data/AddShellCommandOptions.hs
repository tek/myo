module Myo.Command.Data.AddShellCommandOptions where

import Chiasma.Data.Ident (Ident)
import Ribosome (MsgpackDecode, MsgpackEncode)

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Data.CommandId (CommandId)
import Myo.Orphans ()

data AddShellCommandOptions =
  AddShellCommandOptions {
    ident :: CommandId,
    lines :: [Text],
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
  AddShellCommandOptions i l Nothing t Nothing Nothing Nothing Nothing Nothing
