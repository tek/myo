{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tmux.FocusSpec (htf_thisModulesTests) where

import Chiasma.Codec (TmuxCodec)
import Chiasma.Command.Pane (panesAs)
import Chiasma.Data.TmuxId (PaneId)
import Ribosome.Test.Await (await)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)
import Ribosome.Tmux.Run (runTmux)
import Test.Framework

import Myo.Data.Env (Myo)
import Myo.Ui.Default (setupDefaultTestUi)
import Myo.Ui.Focus (myoFocus)
import Myo.Ui.Toggle (myoTogglePane)
import Unit (tmuxSpecDef)

data PaneSelected =
  PaneSelected {
     paneId :: PaneId,
     paneActive :: Bool
  }
  deriving (Eq, Show, Generic, TmuxCodec)

focusPaneSpec :: Myo ()
focusPaneSpec = do
  setupDefaultTestUi
  myoTogglePane "make"
  myoFocus "make"
  await (gassertEqual [PaneSelected 0 False, PaneSelected 1 True]) (runTmux panesAs)

test_focusPane :: IO ()
test_focusPane =
  tmuxGuiSpecDef focusPaneSpec
