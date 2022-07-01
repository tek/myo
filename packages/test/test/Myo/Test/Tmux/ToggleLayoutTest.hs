module Myo.Test.Tmux.ToggleLayoutTest where

import Chiasma.Command.Pane (panes)
import Chiasma.TmuxNative (withTmuxNative_)
import Chiasma.Ui.Data.View (Layout, Pane (Pane), View (View), consLayout, consPane)
import Polysemy.Test (TestError (TestError), UnitTest, assertEq)
import Ribosome.Test (testHandler)

import Myo.Test.Run (myoEmbedTmuxTest)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Data.ViewCoords (viewCoords)
import Myo.Ui.Toggle (myoToggleLayout)
import Myo.Ui.View (createSpace, createWindow, insertLayout, insertPane)

layout :: View Layout
layout = consLayout "l"

pane1 :: View Pane
pane1 = View "p1" def def (Pane False True Nothing)

pane2 :: View Pane
pane2 = consPane "p2"

setupTree ::
  Members [AtomicState UiState, Error TestError] r =>
  Sem r ()
setupTree =
  stopToErrorWith (TestError . show) do
    insertLayout (viewCoords "s" "w" "wroot") layout
    insertPane (viewCoords "s" "w" "l") pane1
    insertPane (viewCoords "s" "w" "l") pane2

test_toggleLayout :: UnitTest
test_toggleLayout =
  myoEmbedTmuxTest $ testHandler do
    _ <- createSpace "s"
    _ <- createWindow (viewCoords "s" "w" "wroot")
    setupTree
    myoToggleLayout ("l")
    assertEq 2 . length =<< withTmuxNative_ panes
