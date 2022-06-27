module Myo.Command.Effect.TmuxMonitor where

import Chiasma.Data.Ident (Ident)
import Chiasma.Data.TmuxId (PaneId)
import Conc (PScoped, pscoped)
import Process (Pid)

data TmuxMonitorTask =
  TmuxMonitorTask {
    ident :: Ident,
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
