module Myo.Command.Proc where

import qualified Conc
import Polysemy.Chronos (ChronosTime)
import qualified Time
import Time (MilliSeconds (MilliSeconds))

import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
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

waitForShell ::
  Members [Executions, Stop RunError, ChronosTime, Race] r =>
  CommandId ->
  Sem r ()
waitForShell ident =
  Conc.timeout_ (stop (RunError.ShellDidntStart ident limit)) limit do
    Time.while (MilliSeconds 100) do
      not <$> Executions.running ident
  where
    limit =
      MilliSeconds 3000
