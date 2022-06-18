module Myo.Command.Data.MonitorEvent where

import Chiasma.Data.Ident (Ident)

data MonitorEvent =
  CommandOutput {
    mecoCommand :: Ident,
    mecoOutput :: ByteString
  }
  |
  Tick
