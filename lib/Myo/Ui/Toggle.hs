module Myo.Ui.Toggle(
  myoTogglePane,
  myoToggleLayout,
) where

import Control.Lens (view)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.State (StateT, runStateT)
import Data.Foldable (traverse_)
import UnliftIO.Directory (getCurrentDirectory)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import qualified Ribosome.Control.Ribo as Ribo (modify, put, state, inspect)
import Ribosome.Msgpack.NvimObject (NO(..))
import Myo.Env (myoSpaces, myoViews)
import Myo.Orphans ()
import Myo.Data.Myo (Myo, Ribo, TVar, Env)
import Myo.Tmux.IO (liftTmux)
import Myo.Ui.Data.Space (Space(Space))
import Myo.Ui.Data.Window (Window(Window))
import Myo.Ui.Render (renderSpace)
import Myo.Ui.Lens.Toggle (envToggleOnePane, envToggleOneLayout)
import qualified Myo.Log as Log

myoRender :: Myo ()
myoRender = do
  cwd <- getCurrentDirectory
  spaces <- myoSpaces
  views <- myoViews
  result <- liftTmux $ runStateT (runExceptT (traverse_ (renderSpace cwd) spaces)) views
  return ()

reportError :: TreeModError -> Myo ()
reportError =
  Log.p

togglePane :: Ident -> ExceptT TreeModError (Ribo (TVar Env)) ()
togglePane ident = do
  env <- lift Ribo.state
  newEnv <- envToggleOnePane ident env
  lift $ Ribo.put newEnv

myoTogglePane :: NO Ident -> Myo ()
myoTogglePane (NO ident) = do
  result <- runExceptT $ togglePane ident
  either reportError (const myoRender) result

toggleLayout :: Ident -> ExceptT TreeModError (Ribo (TVar Env)) ()
toggleLayout ident = do
  env <- lift Ribo.state
  newEnv <- envToggleOneLayout ident env
  lift $ Ribo.put newEnv

myoToggleLayout :: NO Ident -> Myo ()
myoToggleLayout (NO ident) = do
  result <- runExceptT $ toggleLayout ident
  either reportError (const myoRender) result
