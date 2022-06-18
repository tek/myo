module Myo.Test.Tmux.FocusTest where

import Chiasma.Codec (TmuxCodec)
import Chiasma.Command.Pane (panesAs)
import Chiasma.Data.TmuxId (PaneId)
import Ribosome.Test.Await (awaitEqual_)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Tmux (tmuxTestDef)
import Ribosome.Tmux.Run (runTmux)

import Myo.Test.Unit (MyoTest)
import Myo.Ui.Default (setupDefaultTestUi)
import Myo.Ui.Focus (myoFocus)
import Myo.Ui.Toggle (myoTogglePane)

data PaneSelected =
  PaneSelected {
     paneId :: PaneId,
     paneActive :: Bool
  }
  deriving stock (Eq, Show, Generic, TmuxCodec)

focusPaneTest :: MyoTest ()
focusPaneTest = do
  setupDefaultTestUi
  lift (myoTogglePane "make")
  lift (myoFocus "make")
  awaitEqual_ [PaneSelected 0 False, PaneSelected 1 True] (runTmux panesAs)

test_focusPane :: UnitTest
test_focusPane =
  tmuxTestDef focusPaneTest
