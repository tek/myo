module Myo.Env(
  myoViews,
  myoSpaces,
  bracketMyoTempDir,
) where

import Chiasma.Data.Views (Views)
import qualified Control.Lens as Lens (view)
import qualified Ribosome.Control.Ribo as Ribo (inspect)
import System.Directory (getTemporaryDirectory)
import System.FilePath (takeFileName, (</>))
import System.Posix.User (getEffectiveUserName)
import UnliftIO.Directory (getCurrentDirectory, createDirectoryIfMissing)
import UnliftIO.Temporary (withTempDirectory)

import Myo.Data.Myo (Myo)
import Myo.Ui.Data.Space (Space)
import Myo.Ui.View (envSpacesLens, envViewsLens)

myoViews :: Myo Views
myoViews =
  Ribo.inspect $ Lens.view envViewsLens

myoSpaces :: Myo [Space]
myoSpaces =
  Ribo.inspect $ Lens.view envSpacesLens

bracketMyoTempDir :: (FilePath -> IO ()) -> IO ()
bracketMyoTempDir thunk = do
  name <- getEffectiveUserName
  tmp <- getTemporaryDirectory
  let base = tmp </> ("myo-" ++ name)
  createDirectoryIfMissing True base
  project <- takeFileName <$> getCurrentDirectory
  withTempDirectory base (project ++ "-") thunk
