module Myo.Effect.Controller where

import Myo.Command.Data.Command (Command)

data Controller :: Effect where
  RunCommand :: Command -> Controller m ()

makeSem ''Controller
