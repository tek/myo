module Myo.Log where

-- import qualified Chiasma.Data.Views as Views (log)
-- import Chiasma.Ui.ShowTree (printViewTree)
-- import qualified Control.Lens as Lens (toListOf, view)
-- import Prettyprinter (line)
-- import Prettyprinter.Util (putDocW)

-- import Myo.Data.Env (Env)
-- import Myo.Ui.View (envTreesLens, envViewsLens)

-- trees ::
--   (MonadIO m, Member (AtomicState Env) r) =>
--   m ()
-- trees = do
--   ts <- gets $ Lens.toListOf envTreesLens
--   traverse_ printViewTree ts

-- printViewsLog ::
--   (MonadIO m, Member (AtomicState Env) r) =>
--   m ()
-- printViewsLog = do
--   logged <- gets $ Lens.view $ envViewsLens . Views.log
--   liftIO $ traverse_ (putDocW 100) ((<> line) <$> reverse logged)
