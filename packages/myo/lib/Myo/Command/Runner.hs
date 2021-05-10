module Myo.Command.Runner where

import Control.Lens (filtered, firstOf, folded, views)
import Control.Monad.Trans.Control (embed)
import Ribosome.Control.Monad.Ribo (prepend)

import Myo.Command.Data.Command (Command(Command))
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
        fmap (either (Left . RunError.IOEmbed . show) id) <$> run

canRun :: RunTask -> Runner -> Bool
canRun task (Runner _ can _ _ _) =
  can task

explicitRunner ::
  MonadDeepState s Env m =>
  RunTask ->
  m (Maybe Runner)
explicitRunner (RunTask (Command _ _ _ (Just runner) _ _ _ _ _) _ _) =
  gets @Env $ firstOf (Env.runners . folded . filtered matchRunner)
  where
    matchRunner (Runner ident _ _ _ _) =
      ident == runner
explicitRunner _ =
  pure Nothing

runnerForTask :: RunTask -> Env -> Maybe Runner
runnerForTask task =
  views Env.runners $ find (canRun task)

findRunner ::
  MonadDeepError e RunError m =>
  MonadDeepState s Env m =>
  RunTask ->
  m Runner
findRunner task = do
  explicit <- explicitRunner task
  guessed <- gets (runnerForTask task)
  hoistMaybe (RunError.NoRunner task) (explicit <|> guessed)

addRunner ::
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  RunInIO m =>
  Ident ->
  (RunTask -> m (Either RunError ())) ->
  (RunTask -> m (Either RunError (IO ExecutionState))) ->
  CanRun ->
  Maybe (RunTask -> m (Either RunError ByteString)) ->
  m ()
addRunner ident run checkPending can capture = do
  er <- runInIOSE run
  cp <- runInIOSE checkPending
  cpt <- traverse runInIOSE capture
  prepend @Env Env.runners (Runner ident can er cp cpt)

extractRunError ::
  MonadDeepError e RunError m =>
  (RunTask -> m a) ->
  RunTask ->
  m (Either RunError a)
extractRunError run =
  catchAt (return . Left) . (fmap Right . run)
