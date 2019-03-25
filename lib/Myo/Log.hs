module Myo.Log(
  debug,
  info,
  err,
  debugS,
  infoS,
  errS,
  R.p,
  R.prefixed,
  trees,
  printViewsLog,
) where

import qualified Chiasma.Data.Views as Views (log)
import Chiasma.Ui.ShowTree (printViewTree)
import qualified Control.Lens as Lens (toListOf, view)
import Control.Monad.DeepState (MonadDeepState, gets)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.Text.Prettyprint.Doc (line, (<>))
import Data.Text.Prettyprint.Doc.Util (putDocW)
import qualified Ribosome.Log as R (debug, err, info, p, prefixed)

import Myo.Data.Env (Env)
import Myo.Ui.View (envTreesLens, envViewsLens)

debug :: MonadIO m => String -> m ()
debug = R.debug "myo"

info :: MonadIO m => String -> m ()
info = R.info "myo"

err :: MonadIO m => String -> m ()
err = R.err "myo"

debugS :: (MonadIO m, Show a) => a -> m ()
debugS = R.debug "myo"

infoS :: (MonadIO m, Show a) => a -> m ()
infoS = R.info "myo"

errS :: (MonadIO m, Show a) => a -> m ()
errS = R.err "myo"

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
