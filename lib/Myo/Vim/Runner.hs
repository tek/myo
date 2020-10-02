module Myo.Vim.Runner where

import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Data.Views (Views, ViewsError)
import Control.Lens (view)
import Ribosome.Api.Atomic (atomic)
import Ribosome.Msgpack.Error (DecodeError)
import qualified Ribosome.Nvim.Api.Data as ApiData
import Ribosome.Nvim.Api.RpcCall (syncRpcCall)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.Execution (ExecutionState)
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunTask as RunTaskDetails
import Myo.Command.Data.RunTask (RunTask(RunTask))
import Myo.Command.Runner (RunInIO, addRunner)
import Myo.Data.Env (Env)

vimRun ::
  NvimE e m =>
  MonadDeepError e DecodeError m =>
  RunTask ->
  m (Either RunError ())
vimRun (RunTask (view Command.lines -> lines') _ _) =
  Right () <$ atomic (syncRpcCall . ApiData.vimCommand <$> lines')

vimCheckPending :: RunTask -> m (Either RunError (IO ExecutionState))
vimCheckPending =
  undefined

vimCanRun :: RunTask -> Bool
vimCanRun (RunTask _ _ RunTaskDetails.Vim) =
  True
vimCanRun _ =
  False

-- TODO vim can capture
addVimRunner ::
  MonadRibo m =>
  NvimE e m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepState s Views m =>
  MonadDeepError e RunError m =>
  MonadDeepError e ViewsError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepState s CommandState m =>
  RunInIO m =>
  m ()
addVimRunner =
  void $ addRunner (Str "vim") vimRun vimCheckPending vimCanRun Nothing
