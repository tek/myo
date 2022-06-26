module Myo.Command.Kill where

import Chiasma.Data.Ident (Ident)

import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions)
import qualified Myo.Effect.Proc as Proc
import Myo.Effect.Proc (Proc)

killCommand ::
  Members [Executions, Proc !! pe] r =>
  Ident ->
  Sem r ()
killCommand =
  traverse_ (resume_ . Proc.kill) <=< Executions.pid
