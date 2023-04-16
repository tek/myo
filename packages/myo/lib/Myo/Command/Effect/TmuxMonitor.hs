module Myo.Command.Effect.TmuxMonitor where

import Chiasma.Data.TmuxId (PaneId)
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

type ScopedTmuxMonitor =
  Scoped TmuxMonitorTask TmuxMonitor

withTmuxMonitor ::
  Member ScopedTmuxMonitor r =>
  TmuxMonitorTask ->
  InterpreterFor TmuxMonitor r
withTmuxMonitor =
  scoped
