module Myo.Command.Effect.TmuxMonitor where

import Chiasma.Data.TmuxId (PaneId)
import Conc (PScoped, pscoped)
import Process (Pid)

import Myo.Data.CommandId (CommandId)

data TmuxMonitorTask =
  TmuxMonitorTask {
    ident :: CommandId,
    pane :: PaneId,
    shellPid :: Pid
  }
  deriving stock (Eq, Show)

data TmuxMonitor :: Effect where
  Wait :: TmuxMonitor m ()

makeSem ''TmuxMonitor

type ScopedTmuxMonitor res =
  PScoped TmuxMonitorTask res TmuxMonitor

withTmuxMonitor ::
  Member (ScopedTmuxMonitor res) r =>
  TmuxMonitorTask ->
  InterpreterFor TmuxMonitor r
withTmuxMonitor =
  pscoped
