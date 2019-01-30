{-# LANGUAGE RankNTypes #-}

module Myo.Ui.Render(
  renderWindow,
  renderSpace,
  myoRender,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Free.Class (MonadFree)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Except (ExceptT)
import Data.Foldable (traverse_)
import UnliftIO.Directory (getCurrentDirectory)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.RenderError (RenderError)
import qualified Chiasma.Data.RenderError as RenderError (RenderError(..))
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Chiasma.Data.Views (Views)
import Chiasma.Monad.Stream (TmuxProg)
import Chiasma.Render (render)
import Ribosome.Control.Monad.Trans.Ribo (Ribo)
import Ribosome.Control.Monad.RiboE (liftRibo, anaE)
import Ribosome.Control.Monad.State (riboStateLocal)
import Myo.Data.Myo (Env, MyoE)
import Myo.Env (myoSpaces)
import Myo.Tmux.IO (myoTmux)
import Myo.Ui.Data.Space (Space(Space))
import Myo.Ui.Data.Window (Window(Window))
import Myo.Ui.View (envViewsLens)

renderWindow ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  FilePath ->
  Ident ->
  Window ->
  m ()
renderWindow cwd spaceIdent (Window windowIdent tree) =
  render cwd spaceIdent windowIdent tree

renderSpace ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  FilePath ->
  Space ->
  m ()
renderSpace cwd (Space ident windows) =
  traverse_ (renderWindow cwd ident) windows

renderSpaces :: FilePath -> [Space] -> ExceptT RenderError (TmuxProg (Ribo Env)) ()
renderSpaces cwd spaces =
  riboStateLocal envViewsLens $ traverse_ (renderSpace cwd) spaces

myoRender :: MyoE RenderError ()
myoRender = do
  cwd <- getCurrentDirectory
  spaces <- liftRibo myoSpaces
  anaE RenderError.Fatal $ myoTmux $ renderSpaces cwd spaces
