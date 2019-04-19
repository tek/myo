{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module Command.UpdateSpec(
  htf_thisModulesTests,
) where

import Chiasma.Data.Ident (identText)
import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Control.Monad.Trans.Except (ExceptT)
import Data.Default (def)
import qualified Data.Map as Map (singleton)
import Data.MessagePack (Object)
import Neovim (Neovim, Plugin(..))
import Ribosome.Api.Autocmd (doautocmd)
import Ribosome.Api.Variable (setVar)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimGetVar, vimSetVar)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Plugin (riboPlugin, rpcHandler, sync)
import Ribosome.Test.Await (await)
import Ribosome.Test.Embed (integrationSpecDef)
import Ribosome.Test.Orphans ()
import Test.Framework

import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.Command (Command(Command))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (commands)
import Myo.Data.Env (Env(_tempDir))
import Myo.Env (bracketMyoTempDir)
import Myo.Plugin (handleError, variables)
import qualified Myo.Settings as Settings (systemCommands)
import Myo.Ui.Data.UiState (UiState)
import qualified Myo.Ui.Data.UiState as UiState (spaces)

commands1 :: [AddSystemCommandOptions]
commands1 =
  [AddSystemCommandOptions (Ident.Str "c1") ["tail"] Nothing Nothing Nothing]

commands2 :: [AddSystemCommandOptions]
commands2 =
  [
    AddSystemCommandOptions (Ident.Str "c1") ["tails"] Nothing Nothing Nothing,
    AddSystemCommandOptions (Ident.Str "c2") ["echo"] Nothing Nothing Nothing
    ]

cmdData :: MonadDeepState s CommandState m => m [(Text, [Text])]
cmdData =
  fmap extract <$> getL @CommandState CommandState.commands
  where
    extract (Command _ ident lines _ _) = (identText ident, lines)

$(return [])

plugin :: FilePath -> IO (Plugin (Ribosome Env))
plugin tempDir = do
  ribo <- newRibosome "myo" def { _tempDir = tempDir }
  return $ riboPlugin "myo" ribo [$(rpcHandler sync 'cmdData)] [] handleError variables

getCmdData ::
  NvimE e m =>
  m [(Text, [Text])]
getCmdData =
  vimCallFunction "CmdData" []

updateCommandsSpec :: ExceptT RpcError (Neovim ()) ()
updateCommandsSpec = do
  setVar "myo_system_commands" commands1
  doautocmd "CmdlineLeave"
  await (gassertEqual [("c1", ["tail"])]) getCmdData
  setVar "myo_system_commands" commands2
  doautocmd "BufWinEnter"
  await (gassertEqual [("c1", ["tails"]), ("c2", ["echo"])]) getCmdData

test_updateCommands :: IO ()
test_updateCommands =
  bracketMyoTempDir run
  where
    run tempDir = do
      plug <- plugin tempDir
      integrationSpecDef plug updateCommandsSpec
