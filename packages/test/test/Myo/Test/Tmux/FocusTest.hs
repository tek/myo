module Myo.Test.Tmux.FocusTest where

import Chiasma.Codec (TmuxCodec)
import Chiasma.Command.Pane (panes)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Interpreter.Codec (interpretCodecPanes)
import Chiasma.Tmux (withPanes_)
import Polysemy.Test (UnitTest, assertEq)
import Ribosome (mapReport)
import Ribosome.Test (assertWait, testHandler)

import Myo.Command.Interpreter.Backend.Tmux (interpretBackendTmuxNoLog)
import Myo.Data.ViewError (codecError)
import Myo.Interpreter.Controller (interpretControllerTransient)
import Myo.Test.Embed (myoEmbedTmuxTest)
import Myo.Ui.Default (setupDefaultTestUi)
import Myo.Ui.Focus (myoFocus)
import Myo.Ui.Toggle (myoTogglePane)

data PaneSelected =
  PaneSelected {
     paneId :: PaneId,
     paneActive :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (TmuxCodec)

test_focusPane :: UnitTest
test_focusPane =
  myoEmbedTmuxTest $ interpretBackendTmuxNoLog $ interpretControllerTransient [] $
  interpretCodecPanes @PaneSelected $ testHandler do
    setupDefaultTestUi
    myoTogglePane "make"
    myoFocus "make"
    mapReport do
      assertWait (codecError (withPanes_ panes)) (assertEq [PaneSelected 0 False, PaneSelected 1 True])
