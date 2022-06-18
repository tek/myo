module Myo.Command.Data.RunTask where

import Chiasma.Data.Ident (Ident)
import Path (Abs, File, Path)

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
    log :: Path Abs File,
    details :: RunTaskDetails
  }
  deriving stock (Eq, Show)
