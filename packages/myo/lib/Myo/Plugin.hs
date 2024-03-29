module Myo.Plugin where

import Chiasma.Codec.Data (Pane)
import Chiasma.Codec.Data.PaneCoords (PaneCoords)
import Chiasma.Codec.Data.PaneMode (PaneMode)
import Chiasma.Codec.Data.PanePid (PanePid)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.Views (Views)
import Chiasma.Effect.Codec (NativeCodecE, NativeCodecsE)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Interpreter.Codec (interpretCodecPanes, interpretCodecTmuxCommand)
import Chiasma.Interpreter.TmuxClient (interpretTmuxNativeEnvGraceful)
import Conc (Lock, interpretAtomic, interpretLockReentrant, withAsync_)
import Data.MessagePack (Object)
import Exon (exon)
import Options.Applicative (Parser, help, long, option, readerError)
import Options.Applicative.Types (readerAsk)
import Path (parseAbsFile)
import Polysemy.Chronos (ChronosTime)
import Ribosome (
  CompleteStyle (CompleteFiltered),
  Execution (Async, Sync),
  Handler,
  LogReport,
  Persist,
  PersistError,
  PersistPath,
  PersistPathError,
  PluginConfig (PluginConfig),
  RemoteStack,
  Reports,
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
  reportStop,
  resumeLogReport,
  rpc,
  rpcAutocmd,
  rpcCommand,
  rpcFunction,
  runNvimPluginCli,
  watchVariables,
  )
import Ribosome.Menu (ModalWindowMenus, NvimMenus, interpretMenus, interpretWindowMenu)
import qualified Ribosome.Settings as Settings

import Myo.Command.Add (myoAddShellCommand, myoAddSystemCommand)
import Myo.Command.CommandMenu (myoCommands)
import Myo.Command.Data.Command (CommandLanguage)
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Data.LoadHistory (LoadHistory)
import Myo.Command.Data.LogDir (LogDir)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.SocatExe (SocatExe (SocatExe))
import Myo.Command.Data.SocketReaderError (SocketReaderError)
import Myo.Command.Data.StoreHistory (StoreHistory)
import Myo.Command.Edit (EditItem)
import Myo.Command.Effect.Backend (Backend)
import Myo.Command.Effect.CommandLog (CommandLog)
import Myo.Command.Effect.Executions (Executions)
import Myo.Command.Effect.SocketReader (ScopedSocketReader)
import Myo.Command.HistoryMenu (myoHistory)
import Myo.Command.Interpreter.Backend.Generic (interpretBackendFail)
import Myo.Command.Interpreter.Backend.Process (interpretBackendProcessNative)
import Myo.Command.Interpreter.Backend.Tmux (interpretBackendTmuxWithLog)
import Myo.Command.Interpreter.Backend.Vim (interpretBackendVim)
import Myo.Command.Interpreter.CommandLog (interpretCommandLog)
import Myo.Command.Interpreter.Executions (interpretExecutions)
import Myo.Command.Interpreter.SocatExe (interpretReaderSocatExe)
import Myo.Command.Interpreter.SocketReader (interpretSocketReader)
import Myo.Command.Log (myoLogs)
import Myo.Command.Output (myoNext, myoPrev)
import Myo.Command.Parse (myoParse, myoParseLatest)
import Myo.Command.Run (myoLine, myoLineCmd, myoReRun, myoRun)
import Myo.Command.Test (myoTest)
import Myo.Command.Update (fetchCommands, myoUpdateCommands)
import Myo.Command.VimTest (
  myoTestBuildArgs,
  myoTestBuildPosition,
  myoTestDetermineRunner,
  myoTestExecutable,
  myoVimTest,
  )
import Myo.Complete (myoCompleteCommand)
import Myo.Data.CliOptions (CliOptions (CliOptions))
import Myo.Data.CommandId (CommandId)
import Myo.Data.Env (Env)
import Myo.Data.LastSave (LastSave)
import Myo.Data.ProcError (ProcError)
import Myo.Data.SaveLock (SaveLock)
import Myo.Diag (myoDiag)
import Myo.Effect.Commands (Commands)
import Myo.Effect.Controller (Controller)
import qualified Myo.Effect.History as History
import Myo.Effect.History (History)
import Myo.Effect.Outputs (Outputs)
import Myo.Effect.Proc (Proc)
import Myo.Interpreter.Commands (interpretCommands)
import Myo.Interpreter.Controller (interpretController)
import Myo.Interpreter.History (interpretHistory)
import Myo.Interpreter.InputIdent (interpretInputIdentRandom)
import Myo.Interpreter.Outputs (interpretOutputs)
import Myo.Interpreter.Proc (interpretProc)
import Myo.Output.Data.OutputError (OutputError)
import Myo.Output.Data.OutputParser (OutputParser)
import Myo.Output.Effect.Parsing (Parsing)
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
import Myo.Ui.State (myoLayoutState, myoPaneState)
import Myo.Ui.Toggle (myoHideLayout, myoHidePane, myoOpenLayout, myoOpenPane, myoToggleLayout, myoTogglePane)
import Myo.Ui.Update (updateUi)

type MyoStack =
  [
    Executions,
    CommandLog,
    AtomicState Env,
    AtomicState UiState,
    AtomicState Views,
    Reader LogDir,
    Reader (Maybe SocatExe),
    NativeCodecE TmuxCommand,
    NativeCodecE (Panes PaneCoords),
    NativeCodecE (Panes PaneMode),
    NativeCodecE (Panes PanePid),
    NativeCodecE (Panes Pane),
    Proc !! ProcError,
    Backend !! RunError,
    Lock @@ SaveLock,
    Lock @@ StoreHistory,
    Lock @@ LoadHistory,
    Input Ident
  ]

type MyoProdStack =
  [
    Parsing !! OutputError,
    Outputs,
    Controller !! RunError,
    Commands !! CommandError,
    History !! RunError,
    Persist [HistoryEntry] !! PersistError,
    PersistPath !! PersistPathError,
    NativeTmux !! TmuxError,
    ScopedSocketReader !! SocketReaderError,
    AtomicState LastSave,
    ModalWindowMenus CommandId !! RpcError,
    ModalWindowMenus EditItem !! RpcError
  ] ++ NvimMenus ++ MyoStack

outputMappingHandlers ::
  Members [Outputs, Scratch !! RpcError, Rpc !! RpcError, Embed IO] r =>
  [RpcHandler r]
outputMappingHandlers =
  [
    rpcCommand outputQuitName Sync myoOutputQuit,
    rpcCommand outputSelectName Sync myoOutputSelect
  ]

handlers ::
  Members MyoProdStack r =>
  Members [Settings !! SettingError, Scratch !! RpcError, Rpc !! RpcError, DataLog LogReport, Reports] r =>
  Members [ChronosTime, Mask, Log, Resource, Race, Async, Embed IO, Final IO] r =>
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
  rpc "MyoTest" Async myoTest
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
  rpc "MyoHistory" Async myoHistory
  <>
  rpc "MyoCommands" Async myoCommands
  <>
  rpc "MyoOpenPane" Async myoOpenPane
  <>
  rpc "MyoOpenLayout" Async myoOpenLayout
  <>
  rpc "MyoTogglePane" Async myoTogglePane
  <>
  rpc "MyoToggleLayout" Async myoToggleLayout
  <>
  rpc "MyoHidePane" Async myoHidePane
  <>
  rpc "MyoHideLayout" Async myoHideLayout
  <>
  rpc "MyoFocus" Async myoFocus
  <>
  rpc "MyoSave" Async myoSave
  <>
  [
    rpcFunction "MyoRun" Async myoRun,
    rpcFunction "MyoLayoutState" Sync myoLayoutState,
    rpcFunction "MyoPaneState" Sync myoPaneState,
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
  Members [Commands !! CommandError, AtomicState UiState, Settings !! SettingError, Embed IO] r =>
  Map WatchedVariable (Object -> Handler r ())
variables =
  [
    ("myo_commands", myoUpdateCommands),
    ("myo_ui", updateUi)
  ]

prepare ::
  Members (NativeCodecsE [Panes PanePid, Panes PaneCoords]) r =>
  Members [NativeTmux !! TmuxError, AtomicState Views, Commands !! CommandError] r =>
  Members [Resource, Log, History !! RunError] r =>
  Members [Settings !! SettingError, Proc !! ProcError, Rpc !! RpcError, AtomicState UiState, DataLog LogReport] r =>
  Sem r ()
prepare = do
  resumeLogReport @Settings do
    detect <- Settings.get Settings.detectUi
    when detect (reportStop detectDefaultUi)
    resumeLogReport @Commands fetchCommands
    resumeLogReport @History History.load

interpretMyoStack ::
  InterpretersFor MyoStack (RemoteStack CliOptions)
interpretMyoStack =
  interpretInputIdentRandom .
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
  interpretReaderSocatExe .
  interpretLogDir .
  interpretAtomic def .
  interpretAtomic def .
  interpretAtomic def .
  interpretCommandLog (pure 10000) .
  interpretExecutions

parsers ::
  Members [Rpc !! RpcError, Embed IO] r =>
  Map CommandLanguage [OutputParser r]
parsers =
  [
    ("haskell", [haskellOutputParser]),
    ("nix", [nixOutputParser]),
    ("scala", [scalaOutputParser])
  ]

interpretMyoProd ::
  InterpretersFor MyoProdStack (RemoteStack CliOptions)
interpretMyoProd =
  interpretMyoStack .
  interpretWindowMenu .
  interpretMenus .
  interpretMenus .
  interpretAtomic def .
  interpretSocketReader .
  interpretTmuxNativeEnvGraceful Nothing .
  interpretPersistPath True .
  interpretPersist "history" .
  interpretBackendVim .
  interpretBackendProcessNative .
  interpretBackendTmuxWithLog .
  interpretHistory .
  interpretCommands .
  interpretController .
  interpretOutputs .
  interpretParsing parsers .
  withAsync_ prepare

options :: Parser CliOptions
options =
  CliOptions <$> optional (SocatExe <$> option fileParser (long "socat" <> help socatHelp))
  where
    fileParser = do
      raw <- readerAsk
      either (const (readerError [exon|not a valid path: #{raw}|])) pure (parseAbsFile raw)
    socatHelp =
      "The path to the `socat` executable"

conf :: PluginConfig CliOptions
conf =
  PluginConfig "myo" def options

myo :: IO ()
myo =
  runNvimPluginCli @MyoProdStack conf (interpretMyoProd . watchVariables variables) handlers
