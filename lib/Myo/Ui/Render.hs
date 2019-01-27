module Myo.Ui.Render(
  renderWindow,
  renderSpace,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Free.Class (MonadFree)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Except (ExceptT)
import Data.Foldable (traverse_)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Chiasma.Data.Views (Views)
import Chiasma.Render (render)
import Myo.Ui.Data.Space (Space(Space))
import Myo.Ui.Data.Window (Window(Window))

renderWindow ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadIO m) =>
  FilePath ->
  Ident ->
  Window ->
  ExceptT RenderError m ()
renderWindow cwd spaceIdent (Window windowIdent tree) =
  render cwd spaceIdent windowIdent tree

renderSpace ::
  (MonadState Views m, MonadFree TmuxThunk m, MonadIO m) =>
  FilePath ->
  Space ->
  ExceptT RenderError m ()
renderSpace cwd (Space ident windows) = traverse_ (renderWindow cwd ident) windows
