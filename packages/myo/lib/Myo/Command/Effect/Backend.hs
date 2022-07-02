module Myo.Command.Effect.Backend where

import Myo.Command.Data.RunTask (RunTask)

data Backend :: Effect where
  Execute :: RunTask -> Backend m ()
  CaptureOutput :: RunTask -> Backend m ()
  Render :: Backend m ()

makeSem ''Backend
