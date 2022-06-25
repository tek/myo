module Myo.Command.Data.RunningCommand where

import Chiasma.Data.Ident (Ident, Identifiable (..))

import Process (Pid)

data RunningCommand =
  RunningCommand {
    runningCommandIdent :: Ident,
    runningCommandPid :: Pid
  }
  deriving stock (Eq, Show)

instance Identifiable RunningCommand where
  identify =
    runningCommandIdent
