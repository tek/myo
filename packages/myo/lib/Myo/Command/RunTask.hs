module Myo.Command.RunTask where

import Chiasma.Data.Ident (Ident)
import Ribosome (Rpc, RpcError)

import Myo.Command.CommandSpec (compileCommandSpec)
import Myo.Command.Data.Command (Command (..))
import Myo.Command.Data.Param (ParamValues)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask (..))
import Myo.Command.RunTaskDetails (runDetails)
import Myo.Effect.Commands (Commands)

runTask ::
  Members [Rpc !! RpcError, Commands, Stop RunError, Input Ident] r =>
  Command ->
  ParamValues ->
  Sem r RunTask
runTask command overrides = do
  ident <- input
  details <- runDetails command
  (params, compiled) <- compileCommandSpec overrides command.cmdLines
  pure RunTask {..}
