module Myo.Effect.Controller where

import Myo.Command.Data.Command (Command)
import Myo.Command.Data.Param (ParamValues)
import Myo.Data.CommandId (CommandId)

data Controller :: Effect where
  RunCommand :: Command -> ParamValues -> Controller m ()
  CaptureOutput :: CommandId -> Controller m ()

makeSem ''Controller

runCommandDef ::
  Member Controller r =>
  Command ->
  Sem r ()
runCommandDef cmd =
  runCommand cmd mempty
