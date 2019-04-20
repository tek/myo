module Myo.Ui.Toggle where

import Chiasma.Data.Ident (Ident)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import qualified Chiasma.Ui.ViewTree as Chiasma (ensurePaneOpen)
import qualified Control.Lens as Lens (mapMOf)
import Control.Monad.DeepState (modifyM)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor (void)
import Ribosome.Tmux.Run (RunTmux)

import Myo.Orphans ()
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Lens.Toggle (openOnePane, toggleOneLayout, toggleOnePane)
import Myo.Ui.Render (MyoRender, myoRender)
import Myo.Ui.View (uiTreesLens)

toggleView ::
  (MonadDeepError e ToggleError m, MonadDeepState s UiState m) =>
  (Ident -> UiState -> m UiState) ->
  Ident ->
  m ()
toggleView toggle ident =
  put =<< toggle ident =<< get

togglePane ::
  MonadDeepError e TreeModError m =>
  MonadDeepError e ToggleError m =>
  MonadDeepState s UiState m =>
  Ident ->
  m ()
togglePane =
  toggleView toggleOnePane

openPane ::
  MonadDeepError e TreeModError m =>
  MonadDeepError e ToggleError m =>
  MonadDeepState s UiState m =>
  Ident ->
  m ()
openPane =
  toggleView openOnePane

ensurePaneOpen ::
  MonadIO m =>
  MonadDeepError e TreeModError m =>
  MonadDeepError e ToggleError m =>
  MonadDeepState s UiState m =>
  MyoRender s e m =>
  Ident ->
  m ()
ensurePaneOpen ident = do
  modifyM $ Lens.mapMOf uiTreesLens (hoistEither . Chiasma.ensurePaneOpen ident)
  myoRender

myoTogglePane ::
  (MonadDeepError e TreeModError m, MonadDeepError e ToggleError m, RunTmux m, MyoRender s e m) =>
  Ident ->
  m ()
myoTogglePane ident = do
  togglePane ident
  void myoRender

myoToggleLayout ::
  (MonadDeepError e TreeModError m, MonadDeepError e ToggleError m, RunTmux m, MyoRender s e m) =>
  Ident ->
  m ()
myoToggleLayout ident = do
  toggleView toggleOneLayout ident
  void myoRender
