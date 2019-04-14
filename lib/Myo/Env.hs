module Myo.Env where

import Chiasma.Data.Views (Views)
import System.Directory (getTemporaryDirectory)
import System.FilePath (takeFileName, (</>))
import System.Posix.User (getEffectiveUserName)
import UnliftIO.Directory (createDirectoryIfMissing, getCurrentDirectory)
import UnliftIO.Temporary (withTempDirectory)

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

bracketMyoTempDir :: (FilePath -> IO ()) -> IO ()
bracketMyoTempDir thunk = do
  name <- getEffectiveUserName
  tmp <- getTemporaryDirectory
  let base = tmp </> ("myo-" <> name)
  createDirectoryIfMissing True base
  project <- takeFileName <$> getCurrentDirectory
  withTempDirectory base (project <> "-") thunk
