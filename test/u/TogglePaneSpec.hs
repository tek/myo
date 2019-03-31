{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TogglePaneSpec(
  htf_thisModulesTests
) where

import qualified Chiasma.Codec.Data as Codec (Pane)
import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import qualified Chiasma.Monad.Tmux as Tmux (read)
import Chiasma.Ui.Data.View (Layout, Pane, View, consLayoutVertical, consPane)
import Ribosome.Orphans ()
import Ribosome.Tmux.Run (runTmuxE)
import Test.Framework

import Config (vars)
import Myo.Data.Env (MyoN)
import Myo.Ui.Data.ViewCoords (viewCoords)
import Myo.Ui.Toggle (myoTogglePane)
import Myo.Ui.View (createSpace, createWindow, insertLayout, insertPane)
import Unit (tmuxSpecWithDef)

layout :: View Layout
layout = consLayoutVertical (Ident.Str "l")

pane1 :: View Pane
pane1 = consPane (Ident.Str "p1")

pane2 :: View Pane
pane2 = consPane (Ident.Str "p2")

setupTree :: MyoN ()
setupTree = do
  insertLayout (viewCoords "s" "w" "wroot") layout
  insertPane (viewCoords "s" "w" "l") pane1
  insertPane (viewCoords "s" "w" "l") pane2

togglePaneSpec :: MyoN ()
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
  vars >>= tmuxSpecWithDef togglePaneSpec
