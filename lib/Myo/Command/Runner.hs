module Myo.Command.Runner where

import Chiasma.Data.Ident (Ident)
import Control.Lens (Lens')
import qualified Control.Lens as Lens (views)
import Control.Monad.DeepError (MonadDeepError, catchAt, hoistMaybe)
import Control.Monad.DeepState (MonadDeepState, gets)
import Control.Monad.Trans.Control (MonadBaseControl, embed)
import Data.Foldable (find)
import Ribosome.Control.Monad.Ribo (ConcNvimS, RiboE, prepend)

import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(..))
import Myo.Command.Data.RunTask (RunTask(..))
import Myo.Data.Env (CanRun, Env, Runner(Runner))
import qualified Myo.Data.Env as Env (runners)

class MonadBaseControl IO m => RunInIO m where
  runInIOSE :: (RunTask -> m (Either RunError a)) -> m (RunTask -> IO (Either RunError a))

instance Show e => RunInIO (RiboE s e (ConcNvimS s)) where
  runInIOSE =
    fmap catch . embed
    where
      catch run =
        either (Left . RunError.IOEmbed . show) id <$$> run

canRun :: RunTask -> Runner -> Bool
canRun task (Runner _ can _) =
  can task

runnerForTask :: RunTask -> Env -> Maybe Runner
runnerForTask task = Lens.views Env.runners $ find (canRun task)

findRunner ::
  (MonadDeepError e RunError m, MonadDeepState s Env m) =>
  RunTask ->
  m Runner
findRunner task = do
  mayRunner <- gets (runnerForTask task)
  hoistMaybe (RunError.NoRunner task) mayRunner

addRunner ::
  (MonadBaseControl IO m, MonadDeepState s Env m, RunInIO m) =>
  Ident ->
  (RunTask -> m (Either RunError ())) ->
  CanRun ->
  m ()
addRunner ident run can = do
  er <- runInIOSE run
  prepend @Env Env.runners (Runner ident can er)

mkRunner :: MonadDeepError e RunError m => (RunTask -> m ()) -> RunTask -> m (Either RunError ())
mkRunner run =
  catchAt (return . Left) . (fmap Right . run)
