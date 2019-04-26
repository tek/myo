module Myo.Command.Data.MonitorEvent where

import Chiasma.Data.Ident (Ident)
import Data.ByteString (ByteString)

data MonitorEvent =
  CommandOutput {
    mecoCommand :: Ident,
    mecoOutput :: ByteString
  }
  |
  Tick
