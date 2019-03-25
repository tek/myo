module Myo.Command.Runner where

import Chiasma.Data.Ident (Ident)
import Control.Lens (Lens')
import qualified Control.Lens as Lens (views)
import Control.Monad.DeepError (MonadDeepError, hoistMaybe)
import Control.Monad.DeepState (MonadDeepState, gets)
import Control.Monad.Trans.Control (MonadBaseControl(StM), embed)
import Data.Foldable (find)
import Ribosome.Control.Monad.Ribo (prepend)

import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(..))
import Myo.Command.Data.RunTask (RunTask(..))
import Myo.Data.Env (CanRun, Env, Runner(Runner))
import qualified Myo.Data.Env as Env (runners)

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

-- TODO
addRunner ::
  ∀ m s.
  (MonadBaseControl IO m, MonadDeepState s Env m) =>
  Ident ->
  (RunTask -> m ()) ->
  CanRun ->
  m ()
addRunner ident run can = do
  rawRun <- embed run
  prepend (Env.runners :: Lens' Env [Runner]) (Runner ident can (restore . rawRun))
  where
    restore :: IO (StM m ()) -> IO (Either RunError Env)
    restore = undefined
