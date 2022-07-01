module Myo.Command.Data.RunTask where

import Chiasma.Data.Ident (Ident, Identifiable (identify))

import Myo.Command.Data.Command (Command)

data RunTaskDetails =
  Vim
  |
  System
  |
  UiSystem {
    pane :: Ident
  }
  |
  UiShell {
    shell :: Ident,
    pane :: Ident
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
