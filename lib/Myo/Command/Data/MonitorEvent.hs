module Myo.Command.Data.MonitorEvent where

import Chiasma.Data.Ident (Ident)
import Data.ByteString (ByteString)
import Data.Hourglass (Elapsed)

import Myo.Command.Data.Pid (Pid)

data MonitorEvent =
  CommandOutput {
    mecoCommand :: Ident,
    mecoOutput :: ByteString
  }
  |
  CommandPid {
    mecpCommand :: Ident,
    mecpFindPid :: IO (Maybe Pid),
    mecpStartTime :: Elapsed
  }
