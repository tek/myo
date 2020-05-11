{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tmux.TogglePaneSpec(
  htf_thisModulesTests
) where

import qualified Chiasma.Codec.Data as Codec (Pane)
import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import qualified Chiasma.Monad.Tmux as Tmux (read)
import Chiasma.Ui.Data.View (Layout, Pane, View, consLayoutVertical, consPane)
import Prelude hiding (tmuxSpecDef)
import Ribosome.Orphans ()
import Ribosome.Tmux.Run (runTmuxE)
import Test.Framework

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Run (myoRunIdent)
import Myo.Data.Env (Myo)
import Myo.Tmux.Runner (addTmuxRunner)
import Myo.Ui.Data.ViewCoords (viewCoords)
import Myo.Ui.Default (setupDefaultTestUi)
import Myo.Ui.Toggle (myoTogglePane)
import Myo.Ui.View (createSpace, createWindow, insertLayout, insertPane)
import Unit (tmuxSpecDef)

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

togglePaneSpec :: Myo ()
togglePaneSpec = do
  _ <- createSpace (Ident.Str "s")
  _ <- createWindow (viewCoords "s" "w" "wroot")
  setupTree
  myoTogglePane (Ident.Str "p1")
  myoTogglePane (Ident.Str "p2")
  panes <- runTmuxE $ Tmux.read @Codec.Pane "list-panes" ["-a"]
  gassertEqual (Right 3) (fmap length panes)

test_togglePane :: IO ()
test_togglePane =
  tmuxSpecDef togglePaneSpec

shellPanePinSpec :: Myo ()
shellPanePinSpec = do
  setupDefaultTestUi
  addTmuxRunner
  insertPane (viewCoords "vim" "vim" "make") (consPane sid)
  myoAddSystemCommand (AddSystemCommandOptions sid ["tail"] Nothing (Just sid) Nothing Nothing Nothing Nothing)
  myoRunIdent sid
  sleep 3
  panes <- gassertRight =<< runTmuxE (Tmux.read @Codec.Pane "list-panes" ["-a"])
  gassertEqual 3 (length panes)
  where
    sid = Ident.Str "shell"

test_shellPanePin :: IO ()
test_shellPanePin =
  tmuxSpecDef shellPanePinSpec
