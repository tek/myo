module Myo.Command.RunTask where

import Chiasma.Data.Ident (Ident)
import Ribosome (Rpc, RpcError)

import Myo.Command.CommandSpec (compileCommandSpec)
import Myo.Command.Data.Command (Command (..))
import Myo.Command.Data.Param (ParamValues)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask (..))
import Myo.Command.Optparse (OptparseArgs)
import Myo.Command.RunTaskDetails (runDetails)
import Myo.Effect.Commands (Commands)

runTask ::
  Members [Rpc !! RpcError, Commands, Stop RunError, Input Ident] r =>
  Command ->
  ParamValues ->
  Maybe OptparseArgs ->
  Sem r RunTask
runTask command overrides optparseArgs = do
  ident <- input
  details <- runDetails command
  (params, compiled) <- compileCommandSpec overrides optparseArgs command.cmdLines
  pure RunTask {..}
