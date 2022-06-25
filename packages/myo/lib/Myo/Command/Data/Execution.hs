module Myo.Command.Data.Execution where

import Chiasma.Data.Ident (Ident, Identifiable (..))
import qualified Chronos
import Exon (exon)
import Network.Socket (Socket)
import Text.Show (showParen, showsPrec)

import Myo.Command.Data.ExecutionState (ExecutionState)

data ExecutionMonitor =
  ExecutionMonitor {
    state :: ExecutionState,
    startTime :: Chronos.Time,
    socket :: Maybe Socket
  }
  deriving stock (Generic)

instance Show ExecutionMonitor where
  showsPrec d (ExecutionMonitor s t _) =
    showParen (d > 10) [exon|ExecutionMonitor #{showsPrec 11 s} #{showsPrec 11 t}|]

data Execution =
  Execution {
    ident :: Ident,
    log :: ByteString,
    logs :: [ByteString],
    monitor :: ExecutionMonitor
  }
  deriving stock (Show, Generic)

instance Identifiable Execution where
  identify =
    ident
