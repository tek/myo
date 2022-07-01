module Myo.Command.Effect.Executor where

import Myo.Command.Data.RunTask (RunTask)

data Executor :: Effect where
  Execute :: RunTask -> Executor m ()
  CaptureOutput :: RunTask -> Executor m ()

makeSem ''Executor
