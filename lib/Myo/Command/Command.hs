module Myo.Command.Command(
  commandByIdent,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState)
import Chiasma.Data.Ident (Ident)
import Myo.Data.Env (Env)
import Myo.Command.Data.Command (Command)
import Myo.Command.Data.CommandError (CommandError)

commandByIdent ::
  (MonadError CommandError m, MonadState Env m) =>
  Ident ->
  m Command
commandByIdent = undefined
