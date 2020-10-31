module Myo.Plugin where

import qualified Data.Map as Map (fromList)
import Data.MessagePack (Object)
import Myo.Output.Data.OutputError (OutputError)
import Neovim (Neovim, NeovimPlugin, Plugin, wrapPlugin)
import Neovim.Plugin.Classes (AutocmdOptions(AutocmdOptions), CommandOption(CmdComplete))
import Path (Abs, Dir, Path)
import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Control.Ribosome (Ribosome)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Error.Report (reportError)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Plugin (RpcDef, autocmd, autocmdOptions, cmd, riboPlugin, rpcHandler, rpcHandlerDef, sync)
import Ribosome.Plugin.Mapping (MappingHandler, mappingHandler)

import Myo.Command.Add (myoAddShellCommand, myoAddSystemCommand)
import Myo.Command.CommandMenu (myoCommands)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.HistoryMenu (myoHistory)
import Myo.Command.Log (myoLogs)
import Myo.Command.Output (myoNext, myoPrev, outputQuit, outputSelect)
import Myo.Command.Parse (myoParse, myoParseLatest)
import Myo.Command.Run (myo, myoLine, myoLineCmd, myoReRun, myoRun)
import Myo.Command.Test (myoTestBuildArgs, myoTestBuildPosition, myoTestDetermineRunner, myoTestExecutable, myoVimTest)
import Myo.Command.Update (updateCommands)
import Myo.Complete (myoCompleteCommand)
import Myo.Data.Env (Env, Myo)
import Myo.Data.Error (Error)
import Myo.Diag (myoDiag)
import Myo.Init (initialize, myoStage4)
import Myo.Proteome (myoProteomeLoaded)
import Myo.Quit (myoQuit)
import Myo.Save (myoSave)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Focus (myoFocus)
import Myo.Ui.Toggle (myoToggleLayout, myoTogglePane)
import Myo.Ui.Update (updateUi)

handleError :: Error -> Myo ()
handleError =
  reportError "myo"

rpcHandlers :: [[RpcDef (Ribo Env Error)]]
rpcHandlers =
  [
    $(rpcHandler (cmd []) 'myoDiag),
    $(rpcHandler (cmd []) 'myoLogs),
    $(rpcHandlerDef 'myoAddSystemCommand),
    $(rpcHandlerDef 'myoAddShellCommand),
    $(rpcHandlerDef 'myoTogglePane),
    $(rpcHandlerDef 'myoToggleLayout),
    $(rpcHandler (cmd []) 'myoFocus),
    $(rpcHandler (cmd [CmdComplete "custom,MyoCompleteCommand"]) 'myoRun),
    $(rpcHandler sync 'myoCompleteCommand),
    $(rpcHandlerDef 'myoReRun),
    $(rpcHandlerDef 'myoLine),
    $(rpcHandler (cmd [CmdComplete "shellcmd"]) 'myoLineCmd),
    $(rpcHandler (cmd [CmdComplete "shellcmd"]) 'myo),
    $(rpcHandlerDef 'myoParse),
    $(rpcHandler (cmd []) 'myoParseLatest),
    $(rpcHandlerDef 'myoPrev),
    $(rpcHandlerDef 'myoNext),
    $(rpcHandlerDef 'myoSave),
    $(rpcHandlerDef 'myoVimTest),
    $(rpcHandler (cmd []) 'myoHistory),
    $(rpcHandler (cmd []) 'myoCommands),
    $(rpcHandlerDef 'myoStage4),
    $(rpcHandler sync 'myoTestDetermineRunner),
    $(rpcHandler sync 'myoTestExecutable),
    $(rpcHandler sync 'myoTestBuildPosition),
    $(rpcHandler sync 'myoTestBuildArgs),
    $(rpcHandler (autocmd "VimLeavePre" . sync) 'myoQuit),
    $(rpcHandler (autocmd "BufWritePre") 'myoSave),
    $(rpcHandler (
    autocmd "User" . autocmdOptions (AutocmdOptions "ProteomeProject" False Nothing)) 'myoProteomeLoaded)
    ]

mappingOutputQuit ::
  MonadRibo m =>
  NvimE e m =>
  MappingHandler m
mappingOutputQuit =
  mappingHandler "output-quit" outputQuit

mappingOutputSelect ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e OutputError m =>
  MappingHandler m
mappingOutputSelect =
  mappingHandler "output-select" outputSelect

mappings ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e OutputError m =>
  [MappingHandler m]
mappings =
  [mappingOutputQuit, mappingOutputSelect]

variables ::
  MonadRibo m =>
  NvimE e m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepState s CommandState m =>
  MonadDeepState s UiState m =>
  Map Text (Object -> m ())
variables =
  Map.fromList [("myo_commands", updateCommands), ("myo_ui", updateUi)]

plugin' :: Ribosome Env -> Plugin (Ribosome Env)
plugin' env =
  riboPlugin "myo" env rpcHandlers mappings handleError variables

plugin :: Path Abs Dir -> Neovim e NeovimPlugin
plugin =
  wrapPlugin . plugin' <=< initialize
