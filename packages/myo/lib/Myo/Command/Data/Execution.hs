module Myo.Command.Data.Execution where

import Chiasma.Data.Ident (Ident, Identifiable (..))
import qualified Chronos
import Process (Pid)
import Text.Show (show)

import Myo.Command.Data.ExecutionState (ExecutionState (Tracked))

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
    ident :: Ident,
    -- log :: ByteString,
    -- logs :: [ByteString],
    state :: ExecutionState,
    startTime :: Chronos.Time,
    sync :: ExecutionSync
  }
  deriving stock (Show, Generic)

instance Identifiable Execution where
  identify =
    ident

pid ::
  Execution ->
  Maybe Pid
pid = \case
  Execution {state = Tracked p} ->
    Just p
  _ ->
    Nothing
