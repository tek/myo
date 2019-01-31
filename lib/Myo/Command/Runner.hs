module Myo.Command.Runner(
  findRunner,
  addRunner,
) where

import Chiasma.Data.Ident (Ident)
import Chiasma.Data.Maybe (maybeExcept)
import qualified Control.Lens as Lens (views)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState, gets)
import Data.Foldable (find)
import qualified Ribosome.Control.Ribo as Ribo (prepend)
import Ribosome.Control.Monad.State (prepend)

import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(..))
import Myo.Command.Data.RunTask (RunTask(..))
import Myo.Data.Env (Env, Myo, Runner(Runner), RunF, CanRun)
import qualified Myo.Data.Env as Env (_runners)

canRun :: RunTask -> Runner -> Bool
canRun task (Runner _ can _) =
  can task

findRunner ::
  (MonadError RunError m, MonadState Env m) =>
  RunTask ->
  m Runner
findRunner task = do
  mayRunner <- gets $ Lens.views Env._runners $ find (canRun task)
  maybeExcept (RunError.NoRunner task) mayRunner

addRunner ::
  (MonadState Env m) =>
  Ident ->
  RunF ->
  CanRun ->
  m ()
addRunner ident can run =
  prepend Env._runners (Runner ident run can)
