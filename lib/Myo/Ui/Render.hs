{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Myo.Ui.Render where

import Chiasma.Data.Ident (Ident)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Chiasma.Data.Views (Views)
import Chiasma.Render (render)
import Control.Monad ((<=<))
import Control.Monad.DeepError (MonadDeepError, hoistEither)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Free.Class (MonadFree)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Foldable (traverse_)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Tmux.Run (RunTmux, runRiboTmux)

import Myo.Env (myoSpaces)
import Myo.Ui.Data.Space (Space(Space))
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Data.Window (Window(Window))

renderWindow ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  FilePath ->
  Ident ->
  Window ->
  m ()
renderWindow cwd spaceIdent (Window windowIdent tree) =
  render cwd spaceIdent windowIdent tree

renderSpace ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  FilePath ->
  Space ->
  m ()
renderSpace cwd (Space ident windows) =
  traverse_ (renderWindow cwd ident) windows

renderSpaces ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m, MonadError RenderError m) =>
  FilePath ->
  [Space] ->
  m ()
renderSpaces cwd =
  traverse_ (renderSpace cwd)

renderSpacesE ::
  (MonadDeepState s Views m, MonadFree TmuxThunk m) =>
  FilePath ->
  [Space] ->
  m (Either RenderError ())
renderSpacesE cwd =
  runExceptT . renderSpaces cwd

runRenderSpaces ::
  MonadRibo m =>
  MonadDeepError e TmuxError m =>
  MonadDeepError e RenderError m =>
  NvimE e m =>
  MonadDeepState s Views m =>
  RunTmux m =>
  FilePath ->
  [Space] ->
  m ()
runRenderSpaces cwd =
  hoistEither <=< runRiboTmux . renderSpacesE cwd

class (
  MonadRibo m,
  NvimE e m,
  MonadDeepError e TmuxError m,
  MonadDeepError e RenderError m,
  MonadDeepState s Views m,
  MonadDeepState s UiState m,
  RunTmux m
  ) =>
    MyoRender s e m where

instance (
  MonadRibo m,
  NvimE e m,
  MonadDeepError e TmuxError m,
  MonadDeepError e RenderError m,
  MonadDeepState s Views m,
  MonadDeepState s UiState m,
  RunTmux m
  ) =>
    MyoRender s e m where

myoRender :: (MonadIO m, MyoRender s e m) =>
  m ()
myoRender = do
  cwd <- nvimCwd
  spaces <- myoSpaces
  runRenderSpaces cwd spaces
