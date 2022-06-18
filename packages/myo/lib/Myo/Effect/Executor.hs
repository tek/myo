module Myo.Effect.Executor where

import Myo.Command.Data.RunTask (RunTask)

data Executor a :: Effect where
  Accept :: RunTask -> Executor a m (Maybe a)
  Run :: a -> Executor a m (Maybe [Text])

makeSem ''Executor
