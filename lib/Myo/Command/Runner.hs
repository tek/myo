module Myo.Command.Runner(
  findRunner,
  Pid(..),
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask(..))
import Myo.Data.Myo (Env)

newtype Pid =
  Pid Int
  deriving (Eq, Show)

findRunner ::
  (MonadError RunError m, MonadState Env m) =>
  RunTask ->
  m (Maybe Pid)
findRunner = undefined
