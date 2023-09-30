module Myo.Command.Data.RunTask where

import Chiasma.Data.Ident (Ident)

import Myo.Command.Data.Command (Command)
import Myo.Command.Data.Param (ParamValues)
import Myo.Command.Data.UiTarget (UiTarget)
import Myo.Data.CommandId (CommandId)

data RunTaskDetails =
  Vim {
    silent :: Bool,
    target :: Maybe Ident
  }
  |
  System
  |
  UiSystem {
    pane :: UiTarget
  }
  |
  UiShell {
    shell :: CommandId,
    pane :: UiTarget
  }
  deriving stock (Eq, Show)

data RunTask =
  RunTask {
    ident :: Ident,
    command :: Command,
    details :: RunTaskDetails,
    compiled :: [Text],
    params :: ParamValues
  }
  deriving stock (Eq, Show, Generic)
