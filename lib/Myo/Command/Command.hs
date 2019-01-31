module Myo.Command.Command(
  commandByIdent,
  systemCommand,
) where

import qualified Control.Lens as Lens (views)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState, gets)
import Data.Foldable (find)
import Chiasma.Data.Maybe (maybeExcept)
import Chiasma.Data.Ident (Ident, sameIdent)
import Myo.Data.Env (Env)
import qualified Myo.Data.Env as Env (_command)
import qualified Myo.Command.Data.CommandState as CommandState (_commands)
import Myo.Command.Data.Command (Command(Command))
import Myo.Command.Data.CommandInterpreter (CommandInterpreter(System))
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandError as CommandError (CommandError(NoSuchCommand))

commandByIdent ::
  (MonadError CommandError m, MonadState Env m) =>
  Ident ->
  m Command
commandByIdent ident = do
  c <- gets $ Lens.views (Env._command . CommandState._commands) (find $ sameIdent ident)
  maybeExcept (CommandError.NoSuchCommand ident) c

systemCommand :: (Maybe Ident) -> Ident -> [String] -> (Maybe Ident) -> Command
systemCommand target =
  Command (System target)
