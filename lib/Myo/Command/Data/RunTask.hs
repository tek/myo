module Myo.Command.Data.RunTask(
  RunTaskDetails(..),
  RunTask(..),
) where

import Chiasma.Data.Ident (Ident)
import Myo.Command.Data.Command (Command)
import Path (Abs, File, Path)

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
    shell :: Command,
    pane :: Ident
  }
  deriving (Eq, Show)

data RunTask =
  RunTask {
    rtCommand :: Command,
    rtLog :: Path Abs File,
    rtDetails :: RunTaskDetails
  }
  deriving (Eq, Show)
