module Myo.Command.Interpreter.TmuxMonitor where

import Chiasma.Command.Pane (pipePane)
import Chiasma.Data.PipePaneParams (target)
import qualified Chiasma.Data.Target as Target
import Chiasma.Data.TmuxCommand (TmuxCommand (PipePane))
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId, formatId)
import Chiasma.Effect.Codec (NativeCommandCodecE)
import qualified Chiasma.Effect.TmuxApi as Tmux
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Tmux (withTmux_)
import Chiasma.TmuxApi (Tmux)
import Conc (PScoped, interpretPScopedResumableWith, interpretPScopedResumable_, race_)
import Control.Lens.Regex.ByteString (match, regex)
import Data.Char (isAlphaNum)
import qualified Data.Text as Text
import Exon (exon)
import qualified Log
import Path (Abs, File, Path)
import Polysemy.Chronos (ChronosTime)
import Process (Pid, unPid)
import Ribosome (pathText)
import Time (MilliSeconds (MilliSeconds), while)

import qualified Myo.Command.Data.ExecutionState as ExecutionState
import Myo.Command.Data.SocatExe (SocatExe (SocatExe))
import Myo.Command.Data.TmuxMonitorError (TmuxMonitorError (SocketReader, View))
import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions)
import qualified Myo.Command.Effect.SocketReader as SocketReader
import Myo.Command.Effect.SocketReader (ScopedSocketReader, SocketReader, socketReader)
import Myo.Command.Effect.TmuxMonitor (TmuxMonitor (Wait), TmuxMonitorTask (TmuxMonitorTask, ident, pane, shellPid))
import Myo.Data.CommandId (CommandId, commandIdText)
import Myo.Data.ProcError (ProcError)
import Myo.Data.ViewError (ViewError (TmuxApi, TmuxCodec))
import qualified Myo.Effect.Proc as Proc
import Myo.Effect.Proc (Proc)
import Myo.Loop (useWhileJust)
import Myo.Tmux.Proc (commandPid)

type ScopeEffects =
  '[
    SocketReader
  ]

pollPid ::
  Members [Executions, Proc !! ProcError, ChronosTime, Log] r =>
  CommandId ->
  Pid ->
  Sem r ()
pollPid ident shellPid = do
  resumeAs @_ @Proc Nothing (commandPid shellPid) >>= maybe noPid \ pid -> do
    Log.debug [exon|Setting command pid for `#{commandIdText ident}`: #{show (unPid pid)}|]
    Executions.setState ident (ExecutionState.Tracked pid)
    while (MilliSeconds 500) do
      False <! Proc.exists pid
    Log.debug [exon|Process for `#{commandIdText ident}` has terminated|]
  where
    noPid =
      Log.debug [exon|Process for `#{commandIdText ident}` terminated before initial pid was found|]

sanitizeOutput :: ByteString -> ByteString
sanitizeOutput =
  ([regex|\r\n|] . match .~ "\n") . ([regex|(\x{9b}|\x{1b}\[)[0-?]*[ -\/]*[@-~]|] . match .~ "")

readOutput ::
  Members [SocketReader, CommandLog, Log] r =>
  CommandId ->
  Sem r ()
readOutput ident = do
  useWhileJust SocketReader.chunk (CommandLog.append ident . sanitizeOutput)
  Log.debug [exon|Socket for `#{commandIdText ident}` has been closed|]

waitBasic ::
  Members [Executions, Proc !! ProcError, ChronosTime, Race, Log] r =>
  CommandId ->
  Pid ->
  Sem r ()
waitBasic ident shellPid =
  race_ (pollPid ident shellPid) do
    Executions.waitTerminate ident
    Log.debug [exon|Execution terminated for `#{commandIdText ident}`|]

pipePaneToSocket ::
  Member Tmux r =>
  SocatExe ->
  PaneId ->
  Path Abs File ->
  Sem r ()
pipePaneToSocket (SocatExe socat) paneId path =
  pipePane paneId [exon|'#{pathText socat} STDIN UNIX-SENDTO:#{escapedPath}'|]
  where
    escapedPath =
      Text.concatMap escape (pathText path)
    escape c =
      prefix c <> Text.singleton c
    prefix c | isAlphaNum c = ""
    prefix _  = "\\"

pipingToSocket ::
  Members [NativeTmux !! TmuxError, SocketReader, NativeCommandCodecE, Stop ViewError, Resource] r =>
  PaneId ->
  SocatExe ->
  Sem r a ->
  Sem r a
pipingToSocket pane socat =
  resumeHoist TmuxApi . mapStop TmuxCodec . bracket_ acquire release . insertAt @0
  where
    acquire = do
      socket <- SocketReader.path
      withTmux_ do
        pipePaneToSocket socat pane socket
    release =
      withTmux_ do
        Tmux.send (PipePane def { target = Target.Pane pane })

startLog ::
  Member Log r =>
  CommandId ->
  PaneId ->
  Sem r ()
startLog ident pane =
  Log.debug [exon|Monitoring tmux command `#{commandIdText ident}` in `#{formatId pane}`|]

withLog ::
  ∀ socket sre r a .
  Member Resource r =>
  Members [NativeTmux !! TmuxError, ScopedSocketReader socket !! sre, NativeCommandCodecE, Reader (Maybe SocatExe)] r =>
  TmuxMonitorTask ->
  (TmuxMonitorTask -> Sem (SocketReader : Stop (TmuxMonitorError sre) : r) a) ->
  Sem (Stop (TmuxMonitorError sre) : r) a
withLog task@TmuxMonitorTask {..} use = do
  socat <- ask
  resumeHoist SocketReader $ mapStop View do
    socketReader ident $ maybe id (pipingToSocket pane) socat do
      insertAt @1 (use task)

interpretTmuxMonitor ::
  Members [NativeTmux !! TmuxError, Proc !! ProcError, ChronosTime, CommandLog, Reader (Maybe SocatExe)] r =>
  Members [Executions, ScopedSocketReader socket !! sre, NativeCommandCodecE, Resource, Log, Race] r =>
  InterpreterFor (PScoped TmuxMonitorTask TmuxMonitorTask TmuxMonitor !! TmuxMonitorError sre) r
interpretTmuxMonitor =
  interpretPScopedResumableWith @ScopeEffects withLog \ TmuxMonitorTask {..} -> \case
    Wait -> do
      startLog ident pane
      race_ (readOutput ident) (waitBasic ident shellPid)

interpretTmuxMonitorNoLog ::
  Members [Proc !! ProcError, ChronosTime, Executions, Log, Race] r =>
  InterpreterFor (PScoped TmuxMonitorTask TmuxMonitorTask TmuxMonitor !! TmuxMonitorError Void) r
interpretTmuxMonitorNoLog =
  interpretPScopedResumable_ pure \ TmuxMonitorTask {..} -> \case
    Wait -> do
      startLog ident pane
      waitBasic ident shellPid
      Log.debug [exon|tmux monitor for `#{commandIdText ident}` has terminated|]
