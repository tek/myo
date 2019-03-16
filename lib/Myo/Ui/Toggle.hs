module Myo.Ui.Toggle(
  myoTogglePane,
  myoToggleLayout,
  openPane,
  ensurePaneOpen,
) where

import Chiasma.Data.Ident (Ident)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Control.Monad.DeepError (MonadDeepError, hoistEither)
import Control.Monad.DeepState (MonadDeepState(get, put))
import Control.Monad.IO.Class (MonadIO)
import Ribosome.Control.Monad.Ribo (ConcNvimS)
import Ribosome.Error.Report (runRiboReport)
import Ribosome.Msgpack.NvimObject (NO(..))

import Myo.Data.Myo (Env, MyoE)
import Myo.Orphans ()
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Lens.Toggle (envOpenOnePane, envToggleOneLayout, envToggleOnePane)
import Myo.Ui.Render (MyoRender, myoRender)

toggleView ::
  (MonadDeepError e ToggleError m, MonadDeepState s Env m) =>
  (Ident -> Env -> m Env) ->
  Ident ->
  m ()
toggleView toggle ident =
  put =<< toggle ident =<< get

togglePane :: (MonadDeepError e TreeModError m, MonadDeepError e ToggleError m, MonadDeepState s Env m) => Ident -> m ()
togglePane =
  toggleView envToggleOnePane

openPane :: (MonadDeepError e TreeModError m, MonadDeepError e ToggleError m, MonadDeepState s Env m) => Ident -> m ()
openPane =
  toggleView envOpenOnePane

ensurePaneOpen ::
  (
    MonadIO m,
    MonadDeepError e TreeModError (t m),
    MonadDeepError e ToggleError (t m),
    MonadDeepState s Env m,
    MyoRender s e t m
  ) =>
  Ident ->
  t m ()
ensurePaneOpen ident = do
  openPane ident
  hoistEither =<< myoRender

myoTogglePane :: NO Ident -> ConcNvimS Env ()
myoTogglePane (NO ident) =
  runRiboReport "ui" thunk
  where
    thunk :: MyoE ToggleError (ConcNvimS Env) ()
    thunk = togglePane ident

toggleLayout ::
  (MonadDeepError e TreeModError m, MonadDeepError e ToggleError m, MonadDeepState s Env m) =>
  Ident ->
  m ()
toggleLayout =
  toggleView envToggleOneLayout

myoToggleLayout :: NO Ident -> ConcNvimS Env ()
myoToggleLayout (NO ident) =
  runRiboReport "ui" thunk
  where
    thunk :: MyoE ToggleError (ConcNvimS Env) ()
    thunk = toggleLayout ident
