module Myo.Ui.Toggle(
  myoTogglePane,
  myoToggleLayout,
) where

import Chiasma.Data.Ident (Ident)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import qualified Ribosome.Control.Ribo as Ribo (put, state)
import Ribosome.Control.Monad.RiboE (mapE, liftRibo, runRiboReport)
import Ribosome.Msgpack.NvimObject (NO(..))
import Myo.Orphans ()
import Myo.Data.Myo (Myo, MyoE, Env)
import Myo.Ui.Data.ToggleError (ToggleError)
import qualified Myo.Ui.Data.ToggleError as ToggleError (ToggleError(..))
import Myo.Ui.Render (myoRender)
import Myo.Ui.Lens.Toggle (envToggleOnePane, envToggleOneLayout)

toggleView :: (Ident -> Env -> MyoE TreeModError Env) -> Ident -> MyoE ToggleError ()
toggleView toggle ident = do
  env <- liftRibo Ribo.state
  newEnv <- mapE ToggleError.Tree $ toggle ident env
  liftRibo $ Ribo.put newEnv
  mapE ToggleError.Render myoRender

togglePane :: Ident -> MyoE ToggleError ()
togglePane =
  toggleView envToggleOnePane

myoTogglePane :: NO Ident -> Myo ()
myoTogglePane (NO ident) =
  runRiboReport $ togglePane ident

toggleLayout :: Ident -> MyoE ToggleError ()
toggleLayout =
  toggleView envToggleOneLayout

myoToggleLayout :: NO Ident -> Myo ()
myoToggleLayout (NO ident) =
  runRiboReport $ toggleLayout ident
