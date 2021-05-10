module Myo.Command.Data.RunningCommand where

import Chiasma.Data.Ident (Identifiable(..))
import Myo.Command.Data.Pid (Pid)

data RunningCommand =
  RunningCommand {
    runningCommandIdent :: Ident,
    runningCommandPid :: Pid
  }
  deriving (Eq, Show)

instance Identifiable RunningCommand where
  identify = runningCommandIdent
