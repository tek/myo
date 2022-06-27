module Myo.Command.Data.CommandOutput where

import Data.ByteString.Builder (Builder)

data CommandOutput =
  CommandOutput {
    prev :: Maybe (Either Text Builder),
    currentBuilt :: Maybe Text,
    current :: Seq ByteString,
    currentSize :: Int
  }
