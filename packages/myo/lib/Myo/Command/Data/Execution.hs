module Myo.Command.Data.Execution where

import Chiasma.Data.Ident (Identifiable (..))
import qualified Chronos
import Process (Pid)
import Text.Show (show)

import Myo.Command.Data.ExecutionState (ExecutionState (Tracked))
import Myo.Command.Data.UiTarget (UiTarget)
import Myo.Data.CommandId (CommandId)

data ExecutionSync =
  ExecutionSync {
    wait :: MVar (),
    kill :: MVar ()
  }
  deriving stock (Generic)

instance Show ExecutionSync where
  show _ =
    "ExecutionSync"

data Execution =
  Execution {
    ident :: CommandId,
    target :: Maybe UiTarget,
    state :: ExecutionState,
    startTime :: Chronos.Time,
    sync :: ExecutionSync
  }
  deriving stock (Show, Generic)

instance Identifiable Execution where
  identify =
    identify . ident

pid ::
  Execution ->
  Maybe Pid
pid = \case
  Execution {state = Tracked p} ->
    Just p
  _ ->
    Nothing
