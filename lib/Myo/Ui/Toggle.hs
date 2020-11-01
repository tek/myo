module Myo.Ui.Toggle where

import Chiasma.Ui.Data.TreeModError (TreeModError)

import Myo.Orphans ()
import Myo.Ui.Lens.Toggle (openOnePane, toggleOneLayout, toggleOnePane)
import Myo.Ui.Render (MyoRender, myoRender)

ensurePaneOpen ::
  MonadIO m =>
  MonadDeepError e TreeModError m =>
  MyoRender s e m =>
  Ident ->
  m ()
ensurePaneOpen ident = do
  openOnePane ident
  myoRender

myoTogglePane ::
  MonadDeepError e TreeModError m =>
  MyoRender s e m =>
  Ident ->
  m ()
myoTogglePane ident = do
  toggleOnePane ident
  void myoRender

myoToggleLayout ::
  MonadDeepError e TreeModError m =>
  MyoRender s e m =>
  Ident ->
  m ()
myoToggleLayout ident = do
  toggleOneLayout ident
  void myoRender
