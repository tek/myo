module Myo.Command.Kill where

import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions)
import Myo.Data.CommandId (CommandId)
import qualified Myo.Effect.Proc as Proc
import Myo.Effect.Proc (Proc)

terminateCommand ::
  Members [Executions, Proc !! pe] r =>
  CommandId ->
  Sem r ()
terminateCommand =
  traverse_ (resume_ . Proc.term) <=< Executions.pid

killCommand ::
  Members [Executions, Proc !! pe] r =>
  CommandId ->
  Sem r ()
killCommand =
  traverse_ (resume_ . Proc.kill) <=< Executions.pid
