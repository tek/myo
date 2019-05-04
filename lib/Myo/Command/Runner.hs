module Myo.Command.Runner where

import Chiasma.Data.Ident (Ident)
import qualified Control.Lens as Lens (views)
import Control.Monad.DeepError (catchAt, hoistMaybe, MonadDeepError)
import Control.Monad.DeepState (gets, MonadDeepState)
import Control.Monad.Trans.Control (embed, MonadBaseControl)
import Data.Foldable (find)
import Ribosome.Control.Monad.Ribo (prepend, Ribo)

import Myo.Command.Data.Execution (ExecutionState)
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(..))
import Myo.Command.Data.RunTask (RunTask(..))
import Myo.Data.Env (CanRun, Env, Runner(Runner))
import qualified Myo.Data.Env as Env (runners)

class RunInIO m where
  runInIOSE :: (RunTask -> m (Either RunError a)) -> m (RunTask -> IO (Either RunError a))

instance Show e => RunInIO (Ribo s e) where
  runInIOSE =
    fmap catch . embed
    where
      catch run =
        either (Left . RunError.IOEmbed . show) id <$$> run

canRun :: RunTask -> Runner -> Bool
canRun task (Runner _ can _ _) =
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
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  RunInIO m =>
  Ident ->
  (RunTask -> m (Either RunError ())) ->
  (RunTask -> m (Either RunError (IO ExecutionState))) ->
  CanRun ->
  m ()
addRunner ident run checkPending can = do
  er <- runInIOSE run
  cp <- runInIOSE checkPending
  prepend @Env Env.runners (Runner ident can er cp)

extractRunError ::
  MonadDeepError e RunError m =>
  (RunTask -> m a) ->
  RunTask ->
  m (Either RunError a)
extractRunError run =
  catchAt (return . Left) . (fmap Right . run)
