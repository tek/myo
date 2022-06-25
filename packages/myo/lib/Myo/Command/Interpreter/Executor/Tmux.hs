module Myo.Command.Interpreter.Executor.Tmux where

import Chiasma.Codec.Data (Pane)
import Chiasma.Codec.Data.PaneMode (PaneMode)
import Chiasma.Codec.Data.PanePid (PanePid)
import Chiasma.Command.Pane (quitCopyMode, sendKeys)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Ident (Ident, identText)
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.SendKeysParams (Key (Lit))
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId, formatId)
import Chiasma.Data.Views (Views, ViewsError)
import Chiasma.Effect.Codec (NativeCodec, NativeCodecs)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Tmux (withTmuxApis_)
import qualified Chiasma.View as Views
import Exon (exon)
import qualified Log
import Polysemy.Chronos (ChronosTime)
import Ribosome (HostError)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (Command), ident)
import qualified Myo.Command.Data.ExecutionState as ExecutionState
import Myo.Command.Data.Pid (Pid)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask (RunTask), RunTaskDetails (UiShell, UiSystem))
import Myo.Command.Data.TmuxTask (TmuxTask (TmuxTask))
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions)
import Myo.Data.ProcError (ProcError, unProcError)
import qualified Myo.Effect.Executor as Executor
import Myo.Effect.Executor (Executor)
import qualified Myo.Effect.Proc as Proc
import Myo.Effect.Proc (Proc)
import Myo.Tmux.Process (panePid, waitForRunningProcess)
import Myo.Ui.Data.UiState (UiState)

acceptCommand ::
  Bool ->
  PaneId ->
  Command ->
  Maybe TmuxTask
acceptCommand shell pid = \case
  cmd ->
    Just (TmuxTask shell pid cmd)

cmdline :: [Text] -> [Key]
cmdline =
  fmap Lit

paneIdForIdent ::
  Members [AtomicState Views, Stop ViewsError] r =>
  Ident ->
  Sem r PaneId
paneIdForIdent paneIdent =
  stopEither =<< atomicGets (Views.paneId paneIdent)

track :: ()
track =
  undefined

monitor ::
  Members [Proc !! ProcError, Executions, NativeTmux, NativeCodec (Panes PanePid), DataLog HostError, ChronosTime, Log] r =>
  Bool ->
  Ident ->
  PaneId ->
  Pid ->
  Sem r ()
monitor shell ident paneId shellPid = do
  Log.debug [exon|Monitoring tmux command `#{identText ident}` in #{formatId paneId}|]
  unless shell do
    resume_ @ProcError $ Proc.childPid shellPid >>= traverse_ \ pid ->
      Executions.setState ident (ExecutionState.Tracked pid)

type TmuxRunStack =
  NativeCodecs [TmuxCommand, Panes Pane, Panes PaneMode, Panes PanePid] ++ [
    Executions,
    Proc !! ProcError,
    DataLog HostError,
    AtomicState UiState,
    Log,
    ChronosTime,
    Async,
    Embed IO
  ]

runInTmux ::
  ∀ r .
  -- ∀ eres r .
  -- Member (Events eres RunEvent) r =>
  Members TmuxRunStack r =>
  Members [NativeTmux, Stop RunError] r =>
  TmuxTask ->
  Sem r (Maybe [Text])
runInTmux (TmuxTask shell paneId Command {ident, cmdLines}) = do
  mapStop RunError.TmuxCodec do
    shellPid <- withTmuxApis_ @[TmuxCommand, Panes Pane, Panes PaneMode, Panes PanePid] @CodecError do
      shellPid <- panePid paneId
      unless shell do
        Log.debug [exon|Waiting for running process to finish for command `#{identText ident}`|]
        resumeHoist @_ @Proc (RunError.Proc . unProcError) (waitForRunningProcess shellPid)
      quitCopyMode paneId
      Log.debug [exon|Sending command `#{identText ident}` to tmux pane #{formatId paneId}|]
      shellPid <$ sendKeys paneId (cmdline cmdLines)
    monitor shell ident paneId shellPid
    pure Nothing

interpretExecutorTmux ::
  -- Member (Events eres RunEvent) r =>
  Members TmuxRunStack r =>
  Members [NativeTmux !! TmuxError, AtomicState Views] r =>
  InterpreterFor (Executor TmuxTask !! RunError) r
interpretExecutorTmux =
  interpretResumable \case
    Executor.Accept (RunTask cmd _ (UiSystem target)) -> do
      paneId <- mapStop RunError.Views (paneIdForIdent target)
      pure (acceptCommand False paneId cmd)
    Executor.Accept (RunTask cmd _ (UiShell _ target)) -> do
      paneId <- mapStop RunError.Views (paneIdForIdent target)
      pure (acceptCommand True paneId cmd)
    Executor.Accept _ ->
      pure Nothing
    Executor.Run task@(TmuxTask _ _ Command {ident}) ->
      runInTmux task !! \ (e :: TmuxError) -> pure (Just [[exon|tmux execution failed for #{identText ident}|], show e])
