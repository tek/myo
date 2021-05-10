module Myo.Test.Tmux.TogglePaneTest where

import qualified Chiasma.Codec.Data as Codec (Pane)
import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import qualified Chiasma.Monad.Tmux as Tmux (read)
import Chiasma.Ui.Data.View (Layout, Pane, View, consLayoutVertical, consPane)
import Hedgehog (evalEither, (===))
import Ribosome.Orphans ()
import Ribosome.Test.Run (UnitTest)
import Ribosome.Tmux.Run (runTmuxE)

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Run (myoRunIdent)
import Myo.Data.Env (Myo)
import Myo.Test.Unit (MyoTest, tmuxTestDef)
import Myo.Tmux.Runner (addTmuxRunner)
import Myo.Ui.Data.ViewCoords (viewCoords)
import Myo.Ui.Default (setupDefaultTestUi)
import Myo.Ui.Toggle (myoTogglePane)
import Myo.Ui.View (createSpace, createWindow, insertLayout, insertPane)

layout :: View Layout
layout = consLayoutVertical (Ident.Str "l")

pane1 :: View Pane
pane1 = consPane (Ident.Str "p1")

pane2 :: View Pane
pane2 = consPane (Ident.Str "p2")

setupTree :: Myo ()
setupTree = do
  insertLayout (viewCoords "s" "w" "wroot") layout
  insertPane (viewCoords "s" "w" "l") pane1
  insertPane (viewCoords "s" "w" "l") pane2

togglePaneTest :: MyoTest ()
togglePaneTest = do
  _ <- createSpace (Ident.Str "s")
  _ <- createWindow (viewCoords "s" "w" "wroot")
  panes <- lift do
    setupTree
    myoTogglePane (Ident.Str "p1")
    myoTogglePane (Ident.Str "p2")
    runTmuxE (Tmux.read @Codec.Pane "list-panes" ["-a"])
  Right 3 === (length <$> panes)

test_togglePane :: UnitTest
test_togglePane =
  tmuxTestDef togglePaneTest

shellPanePinTest :: MyoTest ()
shellPanePinTest = do
  setupDefaultTestUi
  lift addTmuxRunner
  insertPane (viewCoords "vim" "vim" "make") (consPane sid)
  myoAddSystemCommand (AddSystemCommandOptions sid ["tail"] Nothing (Just sid) Nothing Nothing Nothing Nothing Nothing)
  lift (myoRunIdent sid)
  sleep 3
  panes <- evalEither =<< lift (runTmuxE (Tmux.read @Codec.Pane "list-panes" ["-a"]))
  3 === length panes
  where
    sid = Ident.Str "shell"

test_shellPanePin :: UnitTest
test_shellPanePin =
  tmuxTestDef shellPanePinTest
