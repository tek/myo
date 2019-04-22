module Myo.Env where

import Chiasma.Data.Views (Views)
import Control.Monad.Catch (MonadMask)
import Path (Abs, Dir, Path, dirname, parseRelDir, toFilePath, (</>))
import Path.IO (createDirIfMissing, getCurrentDir, getTempDir, withTempDir)
import System.FilePath (dropTrailingPathSeparator)
import System.Posix.User (getEffectiveUserName)

import Myo.Ui.Data.Space (Space)
import Myo.Ui.Data.UiState (UiState)
import qualified Myo.Ui.Data.UiState as UiState (_spaces)

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
  MonadMask m =>
  MonadIO m =>
  (Path Abs Dir -> m ()) ->
  m ()
bracketMyoTempDir thunk = do
  name <- liftIO getEffectiveUserName
  tmp <- getTempDir
  sub <- parseRelDir ("myo-" <> name)
  let base = tmp </> sub
  createDirIfMissing True base
  project <- dirname <$> getCurrentDir
  withTempDir base (dropTrailingPathSeparator (toFilePath project)) thunk
