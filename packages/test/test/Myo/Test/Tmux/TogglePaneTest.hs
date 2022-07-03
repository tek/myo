module Myo.Test.Tmux.TogglePaneTest where

import qualified Chiasma.Codec.Data.Pane as Chiasma
import qualified Chiasma.Command.Pane as Chiasma
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Tmux (withPanes_)
import Chiasma.Ui.Data.View (Layout, Pane, View, consLayoutVertical, consPane)
import Polysemy.Test (TestError, UnitTest, assert, (===))
import Ribosome (interpretPersistNull)
import Ribosome.Test (assertWait, testHandler)

import Myo.Command.Add (myoAddSystemCommand)
import qualified Myo.Command.Data.AddSystemCommandOptions as AddSystemCommandOptions
import Myo.Command.Data.AddSystemCommandOptions (target)
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Interpreter.Backend.Generic (interpretBackendFail)
import Myo.Command.Interpreter.Backend.Tmux (interpretBackendTmuxNoLog)
import Myo.Command.Interpreter.CommandLog (interpretCommandLogSetting)
import Myo.Command.Run (myoRunIdent)
import Myo.Interpreter.Controller (interpretController)
import Myo.Test.Command (withTestHandlerAsync)
import Myo.Test.Embed (myoEmbedTmuxTest)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Data.ViewCoords (viewCoords)
import Myo.Ui.Default (setupDefaultTestUi)
import Myo.Ui.Toggle (myoTogglePane)
import Myo.Ui.View (createSpace, createWindow, insertLayout, insertPane)

layout :: View Layout
layout =
  consLayoutVertical "l"

pane1 :: View Pane
pane1 =
  consPane "p1"

pane2 :: View Pane
pane2 =
  consPane "p2"

setupTree ::
  Members [AtomicState UiState, Error TestError] r =>
  Sem r ()
setupTree =
  stopToErrorWith show do
    insertLayout (viewCoords "s" "w" "wroot") layout
    insertPane (viewCoords "s" "w" "l") pane1
    insertPane (viewCoords "s" "w" "l") pane2

test_togglePane :: UnitTest
test_togglePane =
  myoEmbedTmuxTest $ interpretBackendFail $ interpretPersistNull $ interpretCommandLogSetting $ interpretBackendTmuxNoLog $ interpretController $
  testHandler do
    _ <- createSpace "s"
    _ <- createWindow (viewCoords "s" "w" "wroot")
    setupTree
    myoTogglePane "p1"
    myoTogglePane "p2"
    panes <- withPanes_ @Chiasma.Pane @CodecError Chiasma.panes
    3 === length panes

test_shellPanePin :: UnitTest
test_shellPanePin =
  myoEmbedTmuxTest $ interpretBackendFail $ interpretPersistNull $ interpretCommandLogSetting $ interpretBackendTmuxNoLog $ interpretController $
  testHandler do
    setupDefaultTestUi
    insertPane (viewCoords "vim" "vim" "make") (consPane sid)
    myoAddSystemCommand (AddSystemCommandOptions.cons sid ["tail"]) { target = Just sid }
    withTestHandlerAsync (myoRunIdent sid) do
      assertWait (Executions.running sid) assert
      panes <- withPanes_ @Chiasma.Pane @CodecError Chiasma.panes
      3 === length panes
      Executions.kill sid
  where
    sid =
      "shell"
