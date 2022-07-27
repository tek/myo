module Myo.Effect.Commands where

import Myo.Command.Data.Command (Command)

data Commands :: Effect where
  Latest :: Commands m Command

makeSem ''Commands
