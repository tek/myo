{-# OPTIONS_GHC -Wno-unused-imports #-}
module Myo.Plugin where

import Chiasma.Data.Views (Views)
import Conc (ChanConsumer, ChanEvents, interpretAtomic, interpretEventsChan, withAsync_)
import Ribosome (
  BootError,
  CompleteStyle (CompleteFiltered),
  Errors,
  Execution (Async),
  HostError,
  Rpc,
  RpcError,
  RpcHandler,
  Scratch,
  SettingError,
  Settings,
  reportError,
  rpc,
  runNvimHandlersIO,
  )
import qualified Ribosome.Settings as Settings

import Myo.Command.Add (myoAddShellCommand, myoAddSystemCommand)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.LogDir (LogDir)
import Myo.Command.Data.RunEvent (RunEvent)
-- import Myo.Command.Run (myoRun)
import Myo.Data.Env (Env)
import Myo.Diag (myoDiag)
import qualified Myo.Settings as Settings
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Default (detectDefaultUi)

-- rpcHandlers :: [[RpcDef (Ribo Env Error)]]
-- rpcHandlers =
--   [
--     $(rpcHandler (cmd []) 'myoLogs),
--     $(rpcHandlerDef 'myoTogglePane),
--     $(rpcHandlerDef 'myoToggleLayout),
--     $(rpcHandler (cmd []) 'myoFocus),
--     $(rpcHandlerDef 'myoReRun),
--     $(rpcHandlerDef 'myoLine),
--     $(rpcHandler (cmd [CmdComplete "shellcmd"]) 'myoLineCmd),
--     $(rpcHandler (cmd [CmdComplete "shellcmd"]) 'myo),
--     $(rpcHandlerDef 'myoParse),
--     $(rpcHandler (cmd []) 'myoParseLatest),
--     $(rpcHandlerDef 'myoPrev),
--     $(rpcHandlerDef 'myoNext),
--     $(rpcHandlerDef 'myoSave),
--     $(rpcHandlerDef 'myoVimTest),
--     $(rpcHandler (cmd []) 'myoHistory),
--     $(rpcHandler (cmd []) 'myoCommands),
--     $(rpcHandlerDef 'myoStage4),
--     $(rpcHandler sync 'myoTestDetermineRunner),
--     $(rpcHandler sync 'myoTestExecutable),
--     $(rpcHandler sync 'myoTestBuildPosition),
--     $(rpcHandler sync 'myoTestBuildArgs),
--     $(rpcHandler (autocmd "VimLeavePre" . sync) 'myoQuit),
--     $(rpcHandler (autocmd "BufWritePre") 'myoSave),
--     $(rpcHandler (
--     autocmd "User" . autocmdOptions (AutocmdOptions "MyoProject" False Nothing)) 'myoMyoLoaded)
--     ]

-- mappingOutputQuit ::
--   MappingHandler m
-- mappingOutputQuit =
--   mappingHandler "output-quit" outputQuit

-- mappingOutputSelect ::
--   MappingHandler m
-- mappingOutputSelect =
--   mappingHandler "output-select" outputSelect

-- mappings ::
--   [MappingHandler m]
-- mappings =
--   [mappingOutputQuit, mappingOutputSelect]

-- variables ::
--   Map Text (Object -> m ())
-- variables =
--   Map.fromList [("myo_commands", updateCommands), ("myo_ui", updateUi)]

type MyoStack =
  [
    AtomicState Env,
    AtomicState UiState,
    AtomicState Views,
    AtomicState CommandState,
    ChanEvents RunEvent,
    ChanConsumer RunEvent,
    Reader LogDir
  ]

handlers ::
  Members MyoStack r =>
  Members [Settings !! se, Scratch !! RpcError, Errors] r =>
  [RpcHandler r]
handlers =
  rpc "ProDiag" Async myoDiag
  <>
  rpc "MyoAddSystemCommand" Async myoAddSystemCommand
  <>
  rpc "MyoAddShellCommand" Async myoAddShellCommand
  -- <>
  -- completeWith CompleteFiltered myoCompleteCommand (rpc "MyoRun" Async myoRun)


prepare ::
  Members [Settings !! SettingError, AtomicState UiState, DataLog HostError] r =>
  Sem r ()
prepare = do
  resuming @_ @Settings (reportError (Just "ui")) do
    detect <- Settings.get Settings.detectUi
    when detect detectDefaultUi

interpretMyoStack ::
  Member (DataLog HostError) r =>
  Members [Rpc !! RpcError, Settings !! SettingError, Error BootError, Race, Log, Resource, Async, Embed IO] r =>
  InterpretersFor MyoStack r
interpretMyoStack sem =
  runReader undefined $
  interpretEventsChan $
  interpretAtomic def $
  interpretAtomic def $
  interpretAtomic def $
  interpretAtomic def do
    withAsync_ prepare sem

myo :: IO ()
myo =
  runNvimHandlersIO @MyoStack "myo" interpretMyoStack handlers
