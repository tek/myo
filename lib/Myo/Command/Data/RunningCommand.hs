module Myo.Command.Data.RunningCommand(
  RunningCommand(..),
) where

import Chiasma.Data.Ident (Ident, Identifiable(..))
import Myo.Command.Data.Pid (Pid)

data RunningCommand =
  RunningCommand {
    runningCommandIdent :: Ident,
    runningCommandPid :: Pid
  }
  deriving (Eq, Show)

instance Identifiable RunningCommand where
  identify = runningCommandIdent
