module Myo.Effect.Controller where

import Myo.Command.Data.Command (Command)
import Myo.Command.Data.Param (ParamValues)
import Myo.Command.Optparse (OptparseArgs)
import Myo.Data.CommandId (CommandId)

data Controller :: Effect where
  RunCommand :: Command -> ParamValues -> Maybe OptparseArgs -> Controller m ()
  CaptureOutput :: CommandId -> Controller m ()

makeSem ''Controller
