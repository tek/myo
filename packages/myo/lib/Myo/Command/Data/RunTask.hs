module Myo.Command.Data.RunTask where

import Chiasma.Data.Ident (Ident, Identifiable (identify))

import Myo.Command.Data.Command (Command)
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
    command :: Command,
    details :: RunTaskDetails
  }
  deriving stock (Eq, Show)

instance Identifiable RunTask where
  identify =
    identify . command
