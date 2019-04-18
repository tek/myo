{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module Tmux.UpdateSpec(
  htf_thisModulesTests,
) where

import Chiasma.Data.Ident (identText)
import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Chiasma.Ui.Data.View (TreeSub(TreeNode, TreeLeaf), View(View))
import qualified Control.Lens as Lens (toListOf)
import Control.Monad.Trans.Except (ExceptT)
import Data.Bifoldable (bifoldMap)
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
import qualified Myo.Ui.Data.AddLayoutOptions as AddLayoutOptions (AddLayoutOptions(layout, ident))
import qualified Myo.Ui.Data.AddPaneOptions as AddPaneOptions (AddPaneOptions(layout, ident))
import Myo.Ui.Data.Space (Space(Space))
import Myo.Ui.Data.UiSettingCodec (UiSettingCodec(UiSettingCodec))
import Myo.Ui.Data.UiState (UiState)
import qualified Myo.Ui.Data.UiState as UiState (spaces)
import Myo.Ui.Data.Window (Window(Window))

ui1 :: UiSettingCodec
ui1 =
  UiSettingCodec
    (Just [def { AddLayoutOptions.ident = Just (Ident.Str "test"), AddLayoutOptions.layout = Ident.Str "vim" }])
    Nothing

ui2 :: UiSettingCodec
ui2 =
  UiSettingCodec
    (Just [def { AddLayoutOptions.ident = Just (Ident.Str "test"), AddLayoutOptions.layout = Ident.Str "make" }])
    (Just [def { AddPaneOptions.ident = Just (Ident.Str "pane"), AddPaneOptions.layout = Ident.Str "test" }])

paneData :: MonadDeepState s UiState m => m [Text]
paneData =
  (>>= extractS) <$> getsL @UiState UiState.spaces
  where
    extractS (Space _ windows) = windows >>= extractW
    extractW (Window _ layout) = bifoldMap viewName viewName layout
    viewName (View ident _ _ _) = [identText ident]

$(return [])

plugin :: FilePath -> IO (Plugin (Ribosome Env))
plugin tempDir = do
  ribo <- newRibosome "myo" def { _tempDir = tempDir }
  return $ riboPlugin "myo" ribo [$(rpcHandler sync 'paneData)] [] handleError variables

getUiData ::
  NvimE e m =>
  m [Text]
getUiData =
  vimCallFunction "PaneData" []

updateUiSpec :: ExceptT RpcError (Neovim ()) ()
updateUiSpec = do
  setVar "myo_ui" ui1
  doautocmd "CmdlineLeave"
  await (gassertEqual ["main", "vim", "test", "vim", "make", "make"]) getUiData
  setVar "myo_ui" ui2
  doautocmd "BufWinEnter"
  await (gassertEqual ["main", "vim", "vim", "make", "test", "pane", "make"]) getUiData

test_updateUi :: IO ()
test_updateUi =
  bracketMyoTempDir run
  where
    run tempDir = do
      plug <- plugin tempDir
      integrationSpecDef plug updateUiSpec
