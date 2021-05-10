module Myo.Env where

import Chiasma.Data.Views (Views)
import Control.Monad.Catch (MonadThrow)
import Path (parseRelDir, toFilePath, (</>), Path, Abs, Dir, parseAbsDir)
import Path.IO (createDirIfMissing, getTempDir)
import System.Posix.User (getEffectiveUserName)

import Myo.Ui.Data.Space (Space)
import Myo.Ui.Data.UiState (UiState)
import qualified Myo.Ui.Data.UiState as UiState (_spaces)
import Chiasma.Test.Tmux (withTempDir)

myoViews ::
  MonadDeepState s Views m =>
  m Views
myoViews =
  get

myoSpaces ::
  MonadDeepState s UiState m =>
  m [Space]
myoSpaces =
  gets UiState._spaces

bracketMyoTempDir ::
  MonadIO m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  (Path Abs Dir -> m a) ->
  m a
bracketMyoTempDir thunk = do
  name <- liftIO getEffectiveUserName
  tmp <- getTempDir
  sub <- parseRelDir ("myo-" <> name)
  let base = tmp </> sub
  createDirIfMissing True base
  withTempDir (toFilePath base) \ fp -> do
    path <- parseAbsDir fp
    thunk path
