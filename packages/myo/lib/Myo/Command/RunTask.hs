module Myo.Command.RunTask where

import Chiasma.Data.Ident (Ident)
import Ribosome (Rpc, RpcError)

import Myo.Command.CommandSpec (compileCommandSpec)
import Myo.Command.Data.Command (Command (..))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.Param (ParamValues)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask (..))
import Myo.Command.RunTaskDetails (runDetails)

runTask ::
  Members [Rpc !! RpcError, AtomicState CommandState, Stop RunError, Input Ident] r =>
  Command ->
  ParamValues ->
  Sem r RunTask
runTask command overrides = do
  ident <- input
  details <- mapStop RunError.Command (runDetails command)
  (params, compiled) <- compileCommandSpec overrides command.cmdLines
  pure RunTask {..}
