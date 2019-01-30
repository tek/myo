module Myo.Command.Data.RunTask(
  RunTaskDetails(..),
  RunTask(..),
) where

import Chiasma.Data.Ident (Ident)
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
    shell :: Command,
    pane :: Ident
  }
  deriving (Eq, Show)

data RunTask =
  RunTask {
    rtCommand :: Command,
    rtLog :: FilePath,
    rtDetails :: RunTaskDetails
  }
  deriving (Eq, Show)
