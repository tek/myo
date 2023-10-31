module Myo.Test.Tmux.StateTest where

import qualified Chiasma.Ui.Data.View
import Polysemy.Test (UnitTest, assert)
import Ribosome.Test (testHandler)

import Myo.Test.Embed (myoEmbedTmuxTest)
import Myo.Ui.Default (setupDefaultTestUi)
import qualified Myo.Ui.State
import Myo.Ui.State (myoLayoutState, myoPaneState)
import Myo.Ui.Toggle (myoToggleLayout)

test_viewState :: UnitTest
test_viewState =
  myoEmbedTmuxTest $ testHandler do
    setupDefaultTestUi
    assert . not . (.open) =<< myoLayoutState "make"
    myoToggleLayout "make"
    assert . (.open) =<< myoLayoutState "make"
    assert . (.extra.open) =<< myoPaneState "make"
