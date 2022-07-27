module Myo.Plugin where

import Chiasma.Codec.Data (Pane)
import Chiasma.Codec.Data.PaneCoords (PaneCoords)
import Chiasma.Codec.Data.PaneMode (PaneMode)
import Chiasma.Codec.Data.PanePid (PanePid)
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.Views (Views)
import Chiasma.Effect.Codec (NativeCodecE, NativeCodecsE)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Interpreter.Codec (interpretCodecPanes, interpretCodecTmuxCommand)
import Chiasma.Interpreter.TmuxClient (interpretTmuxNativeEnvGraceful)
import Conc (Lock, Restoration, interpretAtomic, interpretLockReentrant, withAsync_)
import Data.MessagePack (Object)
import Polysemy.Chronos (ChronosTime)
import Ribosome (
  CompleteStyle (CompleteFiltered),
  Errors,
  Execution (Async, Sync),
  Handler,
  HostError,
  Persist,
  PersistError,
  PersistPath,
  PersistPathError,
  RemoteStack,
  Rpc,
  RpcError,
  RpcHandler,
  Scratch,
  SettingError,
  Settings,
  WatchedVariable,
  completeBuiltin,
  completeWith,
  interpretPersist,
  interpretPersistPath,
  reportError,
  reportStop,
  resumeReportError,
  rpc,
  rpcAutocmd,
  rpcCommand,
  rpcFunction,
  runNvimPluginIO,
  watchVariables,
  )
import Ribosome.Menu (
  MenuState,
  MenusIOEffects,
  NvimMenusIOEffects,
  NvimRenderer,
  interpretMenuRendererNvim,
  interpretMenuStates,
  interpretNvimMenusFinal,
  )
import qualified Ribosome.Settings as Settings

import Myo.Command.Add (myoAddShellCommand, myoAddSystemCommand)
import Myo.Command.CommandMenu (myoCommands)
import Myo.Command.Data.Command (CommandLanguage)
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Data.LoadHistory (LoadHistory)
import Myo.Command.Data.LogDir (LogDir)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.SocketReaderError (SocketReaderError)
import Myo.Command.Data.StoreHistory (StoreHistory)
import Myo.Command.Effect.Backend (Backend)
import Myo.Command.Effect.CommandLog (CommandLog)
import Myo.Command.Effect.Executions (Executions)
import Myo.Command.Effect.SocketReader (ScopedSocketReader)
import Myo.Command.History (loadHistory)
import Myo.Command.HistoryMenu (myoHistory)
import Myo.Command.Interpreter.Backend.Generic (interpretBackendFail)
import Myo.Command.Interpreter.Backend.Process (interpretBackendProcessNative)
import Myo.Command.Interpreter.Backend.Tmux (interpretBackendTmuxWithLog)
import Myo.Command.Interpreter.Backend.Vim (interpretBackendVim)
import Myo.Command.Interpreter.CommandLog (interpretCommandLogSetting)
import Myo.Command.Interpreter.Executions (interpretExecutions)
import Myo.Command.Interpreter.SocketReader (SocketReaderResources, interpretSocketReader)
import Myo.Command.Log (myoLogs)
import Myo.Command.Output (myoNext, myoPrev)
import Myo.Command.Parse (myoParse, myoParseLatest)
import Myo.Command.Run (myoLine, myoLineCmd, myoReRun, myoRun)
import Myo.Command.Test (myoTestBuildArgs, myoTestBuildPosition, myoTestDetermineRunner, myoTestExecutable, myoVimTest)
import Myo.Command.Update (fetchCommands, myoUpdateCommands)
import Myo.Complete (myoCompleteCommand)
import Myo.Data.CommandId (CommandId)
import Myo.Data.Env (Env)
import Myo.Data.LastSave (LastSave)
import Myo.Data.ProcError (ProcError)
import Myo.Data.SaveLock (SaveLock)
import Myo.Diag (myoDiag)
import Myo.Effect.Commands (Commands)
import Myo.Effect.Controller (Controller)
import Myo.Effect.Proc (Proc)
import Myo.Interpreter.Commands (interpretCommands)
import Myo.Interpreter.Controller (interpretController)
import Myo.Interpreter.Proc (interpretProc)
import Myo.Output.Data.OutputError (OutputError)
import Myo.Output.Effect.Parsing (OutputParser, Parsing)
import Myo.Output.Interpreter.Parsing (interpretParsing)
import Myo.Output.Lang.Haskell.Parser (haskellOutputParser)
import Myo.Output.Lang.Nix.Parser (nixOutputParser)
import Myo.Output.Lang.Scala.Parser (scalaOutputParser)
import Myo.Output.ParseReport (myoOutputQuit, myoOutputSelect, outputQuitName, outputSelectName)
import Myo.Quit (myoQuit)
import Myo.Save (myoSave)
import qualified Myo.Settings as Settings
import Myo.Temp (interpretLogDir)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Default (detectDefaultUi)
import Myo.Ui.Focus (myoFocus)
import Myo.Ui.Toggle (myoToggleLayout, myoTogglePane)
import Myo.Ui.Update (updateUi)

type MyoStack =
  [
    Executions,
    AtomicState Env,
    AtomicState UiState,
    AtomicState Views,
    AtomicState CommandState,
    Reader LogDir,
    NativeCodecE TmuxCommand,
    NativeCodecE (Panes PaneCoords),
    NativeCodecE (Panes PaneMode),
    NativeCodecE (Panes PanePid),
    NativeCodecE (Panes Pane),
    Proc !! ProcError,
    Backend !! RunError,
    Lock @@ SaveLock,
    Lock @@ StoreHistory,
    Lock @@ LoadHistory
  ]

type MyoProdStack =
  [
    Parsing !! OutputError,
    Controller !! RunError,
    Commands !! CommandError,
    CommandLog,
    Persist [HistoryEntry] !! PersistError,
    PersistPath !! PersistPathError,
    NativeTmux !! TmuxError,
    ScopedSocketReader SocketReaderResources !! SocketReaderError,
    AtomicState LastSave,
    NvimRenderer CommandId !! RpcError,
    Scoped () (MenuState CommandId)
  ] ++ MenusIOEffects ++ NvimMenusIOEffects ++ MyoStack

outputMappingHandlers ::
  Members [AtomicState CommandState, Scratch !! RpcError, Rpc !! RpcError, Embed IO] r =>
  [RpcHandler r]
outputMappingHandlers =
  [
    rpcCommand outputQuitName Sync myoOutputQuit,
    rpcCommand outputSelectName Sync myoOutputSelect
  ]

handlers ::
  Members MyoProdStack r =>
  Members [Settings !! SettingError, Scratch !! RpcError, Rpc !! RpcError, DataLog HostError, Errors] r =>
  Members [ChronosTime, Mask Restoration, Log, Resource, Race, Async, Embed IO, Final IO] r =>
  [RpcHandler r]
handlers =
  rpc "MyoDiag" Async myoDiag
  <>
  completeWith CompleteFiltered myoCompleteCommand (rpcCommand "MyoRun" Async myoRun)
  <>
  rpc "MyoReRun" Async myoReRun
  <>
  rpc "MyoLine" Async myoLine
  <>
  rpc "MyoVimTest" Async myoVimTest
  <>
  rpc "MyoAddSystemCommand" Async myoAddSystemCommand
  <>
  rpc "MyoAddShellCommand" Async myoAddShellCommand
  <>
  rpc "MyoLogs" Async myoLogs
  <>
  rpc "MyoParse" Async myoParse
  <>
  rpc "MyoParseLatest" Async myoParseLatest
  <>
  rpc "MyoPrev" Async myoPrev
  <>
  rpc "MyoNext" Async myoNext
  <>
  rpc "MyoHistory" Sync myoHistory
  <>
  rpc "MyoCommands" Sync myoCommands
  <>
  rpc "MyoTogglePane" Async myoTogglePane
  <>
  rpc "MyoToggleLayout" Async myoToggleLayout
  <>
  rpc "MyoFocus" Async myoFocus
  <>
  rpc "MyoSave" Async myoSave
  <>
  [
    rpcFunction "MyoRun" Async myoRun,
    completeBuiltin "shellcmd" (rpcCommand "Myo" Async myoLineCmd),
    rpcFunction "MyoTestDetermineRunner" Sync myoTestDetermineRunner,
    rpcFunction "MyoTestExecutable" Sync myoTestExecutable,
    rpcFunction "MyoTestBuildPosition" Sync myoTestBuildPosition,
    rpcFunction "MyoTestBuildArgs" Sync myoTestBuildArgs,
    rpcAutocmd "MyoQuit" Sync "VimLeavePre" def myoQuit,
    rpcAutocmd "MyoSave" Async "BufWritePre" def myoSave
  ] <>
  outputMappingHandlers

variables ::
  Members [AtomicState CommandState, AtomicState UiState, Settings !! SettingError, Embed IO] r =>
  Map WatchedVariable (Object -> Handler r ())
variables =
  [
    ("myo_commands", myoUpdateCommands),
    ("myo_ui", updateUi)
  ]

prepare ::
  Members (NativeCodecsE [Panes PanePid, Panes PaneCoords]) r =>
  Members [NativeTmux !! TmuxError, AtomicState Views, AtomicState CommandState] r =>
  Members [Lock @@ LoadHistory, Resource, Log, Persist [HistoryEntry] !! PersistError] r =>
  Members [Settings !! SettingError, Proc !! ProcError, Rpc !! RpcError, AtomicState UiState, DataLog HostError] r =>
  Sem r ()
prepare = do
  resuming @_ @Settings (reportError (Just "ui")) do
    detect <- Settings.get Settings.detectUi
    when detect (reportStop (Just "ui") detectDefaultUi)
    fetchCommands
    resumeReportError @Rpc (Just "history") (resumeReportError @(Persist _) (Just "history") loadHistory)

interpretMyoStack ::
  InterpretersFor MyoStack RemoteStack
interpretMyoStack =
  interpretLockReentrant . untag .
  interpretLockReentrant . untag .
  interpretLockReentrant . untag .
  interpretBackendFail .
  interpretProc .
  interpretCodecPanes .
  interpretCodecPanes .
  interpretCodecPanes .
  interpretCodecPanes .
  interpretCodecTmuxCommand .
  interpretLogDir .
  interpretAtomic def .
  interpretAtomic def .
  interpretAtomic def .
  interpretAtomic def .
  interpretExecutions

parsers ::
  Member (Embed IO) r =>
  Map CommandLanguage [OutputParser r]
parsers =
  [
    ("haskell", [haskellOutputParser]),
    ("nix", [nixOutputParser]),
    ("scala", [scalaOutputParser])
  ]

interpretMyoProd ::
  InterpretersFor MyoProdStack RemoteStack
interpretMyoProd =
  interpretMyoStack .
  interpretNvimMenusFinal .
  interpretMenuStates .
  interpretMenuRendererNvim .
  interpretAtomic def .
  interpretSocketReader .
  interpretTmuxNativeEnvGraceful Nothing .
  interpretPersistPath True .
  interpretPersist "history" .
  interpretCommandLogSetting .
  interpretBackendVim .
  interpretBackendProcessNative .
  interpretBackendTmuxWithLog .
  interpretCommands .
  interpretController .
  interpretParsing parsers .
  withAsync_ prepare

myo :: IO ()
myo =
  runNvimPluginIO @MyoProdStack "myo" (interpretMyoProd . watchVariables variables) handlers
