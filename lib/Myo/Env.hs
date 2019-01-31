module Myo.Env(
  logError,
  myoViews,
  myoSpaces,
  bracketMyoTempDir,
) where

import Chiasma.Data.Views (Views)
import qualified Control.Lens as Lens (over, view)
import qualified Data.Map.Strict as Map (alter)
import Ribosome.Api.Exists (epochSeconds)
import qualified Ribosome.Control.Ribo as Ribo (modify, inspect)
import Ribosome.Data.Errors (Errors(Errors), Error(Error), ComponentName)
import System.FilePath ((</>), takeFileName)
import System.Posix.Process (getProcessID)
import System.Posix.User (getEffectiveUserName)
import UnliftIO.Directory (getCurrentDirectory)
import UnliftIO.Temporary (withSystemTempDirectory, withTempDirectory)

import qualified Myo.Data.Env as Env (_errors)
import Myo.Data.Myo (Myo)
import Myo.Ui.Data.Space (Space)
import Myo.Ui.View (envSpacesLens, envViewsLens)

storeError :: Int -> ComponentName -> [String] -> Errors -> Errors
storeError time name msg (Errors errors) =
  Errors (Map.alter alter name errors)
  where
    err = Error time msg
    alter Nothing = Just [err]
    alter (Just current) = Just (err:current)

logError :: ComponentName -> [String] -> Myo ()
logError name e = do
  time <- epochSeconds
  Ribo.modify $ Lens.over Env._errors (storeError time name e)

myoViews :: Myo Views
myoViews =
  Ribo.inspect $ Lens.view envViewsLens

myoSpaces :: Myo [Space]
myoSpaces =
  Ribo.inspect $ Lens.view envSpacesLens

bracketMyoTempDir :: (FilePath -> IO ()) -> IO ()
bracketMyoTempDir thunk = do
  name <- getEffectiveUserName
  pid <- getProcessID
  project <- takeFileName <$> getCurrentDirectory
  withSystemTempDirectory ("myo-" ++ name) $ \dir ->
    withTempDirectory dir (project ++ "-" ++ show pid) thunk
