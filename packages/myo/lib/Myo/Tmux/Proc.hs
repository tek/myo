module Myo.Tmux.Proc where

import Chiasma.Codec.Data.PanePid (PanePid (PanePid))
import qualified Chiasma.Data.Panes as Panes
import Chiasma.Data.Panes (TmuxPanes)
import Chiasma.Data.TmuxId (PaneId)
import qualified Chiasma.Effect.TmuxApi as Tmux
import Polysemy.Chronos (ChronosTime)
import Process (Pid (Pid))
import qualified Time
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

-- |Wait for a process's children to settle.
-- Read the children, wait for 100ms and query again.
-- If the process has the same single child before and after waiting, return it.
-- If this check failed for five times, return the potential first child.
commandPid ::
  Members [Proc, ChronosTime] r =>
  Pid ->
  Sem r (Maybe Pid)
commandPid parent =
  spin (1 :: Int) =<< Proc.childPids parent
  where
    spin n children = do
      Time.sleep (MilliSeconds 100)
      check n children =<< Proc.childPids parent
    check _ [old] [new] | old == new =
      pure (Just new)
    check n _ new | n >= 5 =
      pure (head new)
    check n _ new =
      spin (n + 1) new

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
