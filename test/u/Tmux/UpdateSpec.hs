{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tmux.UpdateSpec (htf_thisModulesTests) where

import Chiasma.Data.Ident (identText)
import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Chiasma.Ui.Data.View (View(View))
import Data.Bifoldable (bifoldMap)
import Neovim (Plugin(..))
import Path (Abs, Dir, Path)
import Ribosome.Api.Autocmd (doautocmd)
import Ribosome.Api.Variable (setVar)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Nvim.Api.IO (vimCallFunction)
import Ribosome.Plugin (riboPlugin, rpcHandler, sync)
import Ribosome.Test.Await (await)
import Ribosome.Test.Embed (integrationSpecDef)
import Ribosome.Test.Orphans ()
import Test.Framework

import Myo.Data.Env (Env(_tempDir), Myo)
import Myo.Env (bracketMyoTempDir)
import Myo.Plugin (handleError, variables)
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
  (>>= extractS) <$> getL @UiState UiState.spaces
  where
    extractS (Space _ windows) = windows >>= extractW
    extractW (Window _ layout) = bifoldMap viewName viewName layout
    viewName (View ident _ _ _) = [identText ident]

$(return [])

plugin :: Path Abs Dir -> IO (Plugin (Ribosome Env))
plugin tmp = do
  ribo <- newRibosome "myo" def { _tempDir = tmp }
  return $ riboPlugin "myo" ribo [$(rpcHandler sync 'paneData)] [] handleError variables

getUiData ::
  NvimE e m =>
  m [Text]
getUiData =
  vimCallFunction "PaneData" []

updateUiSpec :: Myo ()
updateUiSpec = do
  setVar "myo_ui" ui1
  doautocmd False "CmdlineLeave"
  await (gassertEqual ["main", "vim", "test", "vim", "make", "make"]) getUiData
  setVar "myo_ui" ui2
  doautocmd False "BufWinEnter"
  await (gassertEqual ["main", "vim", "vim", "make", "test", "pane", "make"]) getUiData

test_updateUi :: IO ()
test_updateUi =
  bracketMyoTempDir run
  where
    run tmp = do
      plug <- plugin tmp
      integrationSpecDef plug updateUiSpec
