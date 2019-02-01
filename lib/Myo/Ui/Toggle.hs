module Myo.Ui.Toggle(
  myoTogglePane,
  myoToggleLayout,
  openPane,
  ensurePaneOpen,
) where

import Chiasma.Data.Ident (Ident)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Ribosome.Control.Monad.RiboE (mapE, liftRibo, runRiboReport)
import qualified Ribosome.Control.Ribo as Ribo (put, state)
import Ribosome.Msgpack.NvimObject (NO(..))

import Myo.Data.Myo (Myo, MyoE, Env)
import Myo.Orphans ()
import Myo.Ui.Data.ToggleError (ToggleError)
import qualified Myo.Ui.Data.ToggleError as ToggleError (ToggleError(..))
import Myo.Ui.Lens.Toggle (envToggleOnePane, envToggleOneLayout, envOpenOnePane)
import Myo.Ui.Render (myoRender)

toggleView :: (Ident -> Env -> MyoE TreeModError Env) -> Ident -> MyoE ToggleError ()
toggleView toggle ident = do
  env <- liftRibo Ribo.state
  newEnv <- mapE ToggleError.Tree $ toggle ident env
  liftRibo $ Ribo.put newEnv
  mapE ToggleError.Render myoRender

togglePane :: Ident -> MyoE ToggleError ()
togglePane =
  toggleView envToggleOnePane

openPane :: Ident -> MyoE ToggleError ()
openPane =
  toggleView envOpenOnePane

ensurePaneOpen :: Ident -> MyoE ToggleError ()
ensurePaneOpen ident = do
  openPane ident
  mapE ToggleError.Render myoRender

myoTogglePane :: NO Ident -> Myo ()
myoTogglePane (NO ident) =
  runRiboReport "ui" $ togglePane ident

toggleLayout :: Ident -> MyoE ToggleError ()
toggleLayout =
  toggleView envToggleOneLayout

myoToggleLayout :: NO Ident -> Myo ()
myoToggleLayout (NO ident) =
  runRiboReport "ui" $ toggleLayout ident
