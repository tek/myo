module Myo.Command.Interpreter.Backend.Vim where

import qualified Ribosome as Rpc
import Ribosome (Rpc, RpcError, silentBang)
import Ribosome.Host.Api.Data (nvimCommand)

import Myo.Command.Data.Command (Command (Command), cmdLines)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask (RunTask), RunTaskDetails (Vim))
import Myo.Command.Data.VimTask (VimTask (VimTask))
import Myo.Command.Effect.Backend (Backend)
import Myo.Command.Interpreter.Backend.Generic (captureUnsupported, interceptBackend)

acceptVim ::
  RunTask ->
  Sem r (Maybe VimTask)
acceptVim = \case
  RunTask Command {cmdLines} (Vim silent target) ->
    pure (Just (VimTask cmdLines silent target))
  _ ->
    pure Nothing

runInVim ::
  Members [Rpc !! RpcError, Stop RunError] r =>
  VimTask ->
  Sem r ()
runInVim (VimTask cmds silent _) =
  resumeHoist RunError.Rpc do
    (if silent then silentBang else id) do
      Rpc.sync (foldMap nvimCommand cmds)

interpretBackendVim ::
  Members [Backend !! RunError, Rpc !! RpcError] r =>
  Sem r a ->
  Sem r a
interpretBackendVim =
  interceptBackend acceptVim runInVim (captureUnsupported "vim") unit
