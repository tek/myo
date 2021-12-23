module Myo.Log (
  module Myo.Log,
  module Ribosome.Log,
) where

import qualified Chiasma.Data.Views as Views (log)
import Chiasma.Ui.ShowTree (printViewTree)
import qualified Control.Lens as Lens (toListOf, view)
import Prettyprinter (line)
import Prettyprinter.Util (putDocW)
import Ribosome.Log (debug, err, info, prefixed)

import Myo.Data.Env (Env)
import Myo.Ui.View (envTreesLens, envViewsLens)

trees ::
  (MonadIO m, MonadDeepState s Env m) =>
  m ()
trees = do
  ts <- gets $ Lens.toListOf envTreesLens
  traverse_ printViewTree ts

printViewsLog ::
  (MonadIO m, MonadDeepState s Env m) =>
  m ()
printViewsLog = do
  logged <- gets $ Lens.view $ envViewsLens . Views.log
  liftIO $ traverse_ (putDocW 100) ((<> line) <$> reverse logged)
