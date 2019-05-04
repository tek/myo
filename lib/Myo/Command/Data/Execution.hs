{-# LANGUAGE TemplateHaskell #-}

module Myo.Command.Data.Execution where

import Chiasma.Data.Ident (Ident, Identifiable(..))
import Control.Lens (makeClassy)
import Data.ByteString (ByteString)
import Data.Hourglass (Elapsed)
import Network.Socket (Socket)
import qualified Text.Show

import Myo.Command.Data.Pid (Pid)

data ExecutionState =
  Pending
  |
  Running
  |
  Starting Pid
  |
  Tracked Pid
  |
  Stopped
  |
  Unknown
  deriving (Eq, Show)

data ExecutionMonitor =
  ExecutionMonitor {
    _state :: ExecutionState,
    _startTime :: Elapsed,
    _socket :: Maybe Socket,
    _checkPending :: IO ExecutionState
  }

makeClassy ''ExecutionMonitor

instance Text.Show.Show ExecutionMonitor where
  show (ExecutionMonitor s t _ _) =
    "ExecutionMonitor" <> show (s, t)

data Execution =
  Execution {
    _ident :: Ident,
    _log :: ByteString,
    _logs :: [ByteString],
    _monitor :: ExecutionMonitor
  }
  deriving Show

makeClassy ''Execution

instance Identifiable Execution where
  identify = _ident
