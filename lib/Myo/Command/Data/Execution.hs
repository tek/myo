{-# LANGUAGE TemplateHaskell #-}

module Myo.Command.Data.Execution where

import Chiasma.Data.Ident (Ident, Identifiable(..))
import Control.Lens (makeClassy)
import Data.ByteString (ByteString)
import Data.Hourglass (Elapsed)

import Myo.Command.Data.Pid (Pid)

data ExecutionState =
  Pending
  |
  Running
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
    _checkPending :: Ident -> IO ExecutionState
  }

makeClassy ''ExecutionMonitor

data Execution =
  Execution {
    _ident :: Ident,
    _log :: ByteString,
    _logs :: [ByteString],
    _monitor :: ExecutionMonitor
  }

makeClassy ''Execution

instance Identifiable Execution where
  identify = _ident
