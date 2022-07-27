module Myo.Effect.Controller where

import Myo.Command.Data.Command (Command)
import Myo.Data.CommandId (CommandId)

data Controller :: Effect where
  RunIdent :: CommandId -> Controller m ()
  RunCommand :: Command -> Controller m ()
  CaptureOutput :: CommandId -> Controller m ()

makeSem ''Controller
