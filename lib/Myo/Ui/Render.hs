{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Myo.Ui.Render where

import Chiasma.Data.Ident (Ident)
import Chiasma.Data.RenderError (RenderError)
import qualified Chiasma.Data.RenderError as RenderError (RenderError(..))
import Chiasma.Data.TmuxThunk (TmuxError, TmuxThunk)
import Chiasma.Data.Views (Views)
import Chiasma.Render (render)
import Control.Monad (join)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.DeepState (MonadDeepState(stateM))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Free.Class (MonadFree)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (runStateT)
import Data.Either.Combinators (mapLeft)
import Data.Foldable (traverse_)
import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim)
import UnliftIO.Directory (getCurrentDirectory)

import Myo.Env (myoSpaces)
import Myo.Tmux.IO (RunTmux, runMyoTmux)
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
  stateM $ runStateT $ render cwd spaceIdent windowIdent tree

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
  (MonadRibo m, Nvim m, MonadDeepState s Views m, RunTmux m) =>
  FilePath ->
  [Space] ->
  m (Either RenderError ())
runRenderSpaces cwd =
  fmap (join . mapLeft RenderError.Fatal) <$> runMyoTmux . renderSpacesE cwd

class (
  MonadTrans t,
  MonadRibo m,
  MonadDeepError e TmuxError (t m),
  MonadDeepError e RenderError (t m),
  Nvim m,
  MonadDeepState s Views m,
  MonadDeepState s UiState (t m),
  RunTmux m
  ) =>
    MyoRender s e t m where

instance (
  MonadTrans t,
  MonadRibo m,
  MonadDeepError e TmuxError (t m),
  MonadDeepError e RenderError (t m),
  Nvim m,
  MonadDeepState s Views m,
  MonadDeepState s UiState (t m),
  RunTmux m
  ) =>
    MyoRender s e t m where

myoRender :: (MonadIO m, MyoRender s e t m) =>
  t m (Either RenderError ())
myoRender = do
  cwd <- lift getCurrentDirectory
  spaces <- myoSpaces
  lift $ runRenderSpaces cwd spaces
