module Myo.Tmux.Process where

import Chiasma.Codec.Data.PanePid (PanePid (PanePid))
import qualified Chiasma.Data.Panes as Panes
import Chiasma.Data.Panes (TmuxPanes)
import Chiasma.Data.TmuxId (PaneId)
import qualified Chiasma.Effect.TmuxApi as Tmux
import Control.Monad.Extra (whileM)
import Polysemy.Chronos (ChronosTime)
import qualified Time
import Time (MilliSeconds (..))

import Myo.Command.Data.Pid (Pid (Pid))
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
-- If the process has no children, return 'Nothing' immediately.
-- If the process has children, wait for 100ms and query again.
-- If the process has the same single child before and after waiting, return it.
-- If this check failed for five times, return the potential first child.
commandPid ::
  Members [Proc, ChronosTime] r =>
  Pid ->
  Sem r (Maybe Pid)
commandPid parent =
  spin (1 :: Int) =<< Proc.childPids parent
  where
    spin _ [] =
      pure Nothing
    spin n children = do
      Time.sleep (MilliSeconds 100)
      check n children =<< Proc.childPids parent
    check _ [old] [new] | old == new =
      pure (Just new)
    check n _ new | n >= 5 =
      pure (head new)
    check n _ new =
      spin (n + 1) new

waitForRunningProcess ::
  Members [Proc, ChronosTime] r =>
  Pid ->
  Sem r ()
waitForRunningProcess parent =
  whileM do
    isJust <$> commandPid parent
