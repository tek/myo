module Myo.Env(
  logError,
  myoViews,
  myoSpaces,
) where

import qualified Control.Lens as Lens (over, view)
import qualified Data.Map.Strict as Map (adjust)
import Chiasma.Data.Views (Views)
import Ribosome.Api.Exists (epochSeconds)
import qualified Ribosome.Control.Ribo as Ribo (modify, inspect)
import Ribosome.Data.Errors (Errors(Errors), Error(Error), ComponentName)
import Myo.Data.Myo (Myo)
import qualified Myo.Data.Env as Env (_errors)
import Myo.Ui.Data.Space (Space)
import Myo.Ui.View (envSpacesLens, envViewsLens)

storeError :: Int -> ComponentName -> [String] -> Errors -> Errors
storeError time name msg (Errors errors) =
  Errors (Map.adjust (err:) name errors)
  where
    err = Error time msg

logError :: ComponentName -> String -> Myo ()
logError name e = do
  time <- epochSeconds
  Ribo.modify $ Lens.over Env._errors (storeError time name [e])

myoViews :: Myo Views
myoViews =
  Ribo.inspect $ Lens.view envViewsLens

myoSpaces :: Myo [Space]
myoSpaces =
  Ribo.inspect $ Lens.view envSpacesLens
