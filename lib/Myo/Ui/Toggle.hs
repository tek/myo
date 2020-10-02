module Myo.Ui.Toggle where

import Chiasma.Ui.Data.TreeModError (TreeModError)
import Ribosome.Tmux.Run (RunTmux)

import Myo.Orphans ()
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Lens.Toggle (openOnePane, toggleOneLayout, toggleOnePane)
import Myo.Ui.Render (MyoRender, myoRender)

ensurePaneOpen ::
  MonadIO m =>
  MonadDeepError e TreeModError m =>
  MonadDeepError e ToggleError m =>
  MonadDeepState s UiState m =>
  MyoRender s e m =>
  Ident ->
  m ()
ensurePaneOpen ident = do
  openOnePane ident
  myoRender

myoTogglePane ::
  MonadDeepError e TreeModError m =>
  MonadDeepError e ToggleError m =>
  MonadIO m =>
  RunTmux m =>
  MyoRender s e m =>
  Ident ->
  m ()
myoTogglePane ident = do
  toggleOnePane ident
  void myoRender

myoToggleLayout ::
  MonadDeepError e TreeModError m =>
  MonadDeepError e ToggleError m =>
  MonadIO m =>
  RunTmux m =>
  MyoRender s e m =>
  Ident ->
  m ()
myoToggleLayout ident = do
  toggleOneLayout ident
  void myoRender
