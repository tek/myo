{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tmux.ToggleLayoutSpec (htf_thisModulesTests) where

import qualified Chiasma.Codec.Data as Codec (Pane)
import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import qualified Chiasma.Monad.Tmux as Tmux (read)
import Chiasma.Ui.Data.View (Layout, Pane, View, consLayout, consPane)
import Ribosome.Tmux.Run (runTmuxE)
import Test.Framework

import Myo.Data.Env (Myo)
import Myo.Ui.Data.ViewCoords (viewCoords)
import Myo.Ui.Toggle (myoToggleLayout)
import Myo.Ui.View
import Unit (tmuxSpecDef)

layout :: View Layout
layout = consLayout (Ident.Str "l")

pane1 :: View Pane
pane1 = consPane (Ident.Str "p1")

setupTree :: Myo ()
setupTree = do
  insertLayout (viewCoords "s" "w" "wroot") layout
  insertPane (viewCoords "s" "w" "l") pane1

toggleLayoutSpec :: Myo ()
toggleLayoutSpec = do
  _ <- createSpace (Ident.Str "s")
  _ <- createWindow (viewCoords "s" "w" "wroot")
  setupTree
  myoToggleLayout (Ident.Str "l")
  panes <- runTmuxE $ Tmux.read @Codec.Pane "list-panes" ["-a"]
  gassertEqual (Right 2) (fmap length panes)

test_toggleLayout :: IO ()
test_toggleLayout =
  tmuxSpecDef toggleLayoutSpec
