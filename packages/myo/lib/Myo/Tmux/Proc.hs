module Myo.Tmux.Proc where

import Chiasma.Codec.Data.PanePid (PanePid (PanePid))
import qualified Chiasma.Data.Panes as Panes
import Chiasma.Data.Panes (TmuxPanes)
import Chiasma.Data.TmuxId (PaneId)
import qualified Chiasma.Effect.TmuxApi as Tmux
import Polysemy.Chronos (ChronosTime)
import Process (Pid (Pid))
import Time (MilliSeconds (..), while)

import qualified Myo.Effect.Proc as Proc
import Myo.Effect.Proc (Proc)

panePid ::
  Member (TmuxPanes PanePid) r =>
  PaneId ->
  Sem r Pid
panePid paneId = do
  PanePid _ pid <- Tmux.send (Panes.Get paneId)
  pure (Pid pid)

leafPid ::
  Member Proc r =>
  Pid ->
  Sem r Pid
leafPid parent =
  fromMaybe parent . head <$> spin parent
  where
    spin p =
      Proc.childPids p >>= \case
        [] -> pure [p]
        cs ->
          join <$> traverse spin cs

shellBusy ::
  Member Proc r =>
  Pid ->
  Sem r Bool
shellBusy shellPid =
  not . null <$> Proc.childPids shellPid

waitForRunningProcess ::
  Members [Proc, ChronosTime] r =>
  Pid ->
  Sem r ()
waitForRunningProcess parent =
  while (MilliSeconds 100) (shellBusy parent)
