module Myo.Command.Run(
  myoRun,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState)
import Neovim (Neovim)
import Chiasma.Data.Ident (Ident)
import Ribosome.Msgpack.NvimObject (NO(..))
import Ribosome.Control.Monad.Ribo (MonadRibo(..), MonadRiboError(..))
import Ribosome.Control.Ribosome (Ribosome)
import Myo.Orphans ()
import Myo.Command.Command (commandByIdent)
import Myo.Command.Data.Command (Command(..))
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(..))
import Myo.Command.History (pushHistory)
import Myo.Command.RunTask (runTask)
import Myo.Command.Runner (Pid, findRunner)
import Myo.Data.Myo (Myo, MyoT, TVar, Env)
import qualified Myo.Log as Log

reportError :: RunError -> Myo ()
reportError =
  Log.p

storeRunningCommand ::
  (MonadError RunError m, MonadState Env m) =>
  Command ->
  Maybe Pid ->
  m ()
storeRunningCommand = undefined

runCommand ::
  (MonadError RunError m, MonadState Env m) =>
  Command -> m ()
runCommand cmd = do
  task <- runTask cmd
  pid <- findRunner task
  storeRunningCommand cmd pid
  pushHistory cmd

runIdent ::
  Ident ->
  MyoT RunError ()
runIdent ident = do
  cmd <- mapE RunError.Command cmdResult
  runCommand cmd
  where
    cmdResult = commandByIdent ident

myoRun :: NO Ident -> Neovim (Ribosome (TVar Env)) ()
myoRun (NO ident) = do
  result <- asNeovim $ runIdent ident
  either reportError (const $ return ()) result
