module Myo.Test.Tmux.UpdateTest where

import qualified Chiasma.Data.Ident as Ident (Ident (Str))
import Chiasma.Data.Ident (identText)
import Chiasma.Ui.Data.View (View (View))
import Data.Bifoldable (bifoldMap)
import Polysemy.Test (UnitTest, assertEq)
import Ribosome (Execution (Sync), Handler, Rpc, rpcFunction, watchVariables)
import Ribosome.Api (doautocmd, nvimCallFunction, nvimSetVar)
import Ribosome.Host.Interpreter.Handlers (withHandlers)
import Ribosome.Test (assertWait, testPluginEmbed)

import Myo.Plugin (variables)
import Myo.Test.Run (runMyoTestStack)
import qualified Myo.Ui.Data.AddLayoutOptions as AddLayoutOptions (AddLayoutOptions (ident, layout))
import qualified Myo.Ui.Data.AddPaneOptions as AddPaneOptions (AddPaneOptions (ident, layout))
import Myo.Ui.Data.Space (Space (Space))
import Myo.Ui.Data.UiSettingCodec (UiSettingCodec (UiSettingCodec))
import Myo.Ui.Data.UiState (UiState)
import qualified Myo.Ui.Data.UiState as UiState (spaces)
import Myo.Ui.Data.Window (Window (Window))

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

paneData ::
  Member (AtomicState UiState) r =>
  Handler r [Text]
paneData =
  (>>= extractS) <$> atomicGets UiState.spaces
  where
    extractS (Space _ windows) = windows >>= extractW
    extractW (Window _ layout) = bifoldMap viewName viewName layout
    viewName (View ident _ _ _) = [identText ident]

getUiData ::
  Member Rpc r =>
  Sem r [Text]
getUiData =
  nvimCallFunction "PaneData" []

test_updateUi :: UnitTest
test_updateUi =
  runMyoTestStack def $ withHandlers [rpcFunction "PaneData" Sync paneData] $ watchVariables variables $ testPluginEmbed do
    nvimSetVar "myo_ui" ui1
    doautocmd "CmdlineLeave"
    assertWait getUiData (assertEq ["main", "vim", "test", "vim", "make", "make", "scratch"])
    nvimSetVar "myo_ui" ui2
    doautocmd "BufWinEnter"
    assertWait getUiData (assertEq ["main", "vim", "vim", "make", "test", "pane", "make", "scratch"])
