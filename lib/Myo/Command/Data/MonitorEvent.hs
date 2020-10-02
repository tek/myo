module Myo.Command.Data.MonitorEvent where

data MonitorEvent =
  CommandOutput {
    mecoCommand :: Ident,
    mecoOutput :: ByteString
  }
  |
  Tick
