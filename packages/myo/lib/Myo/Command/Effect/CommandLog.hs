module Myo.Command.Effect.CommandLog where

import Chiasma.Data.Ident (Ident)

data CommandLog :: Effect where
  Append :: Ident -> ByteString -> CommandLog m ()
  Archive :: Ident -> CommandLog m ()
  Chunks :: Ident -> CommandLog m (Maybe (Seq ByteString))
  Get :: Ident -> CommandLog m (Maybe Text)
  GetPrev :: Ident -> CommandLog m (Maybe Text)

makeSem ''CommandLog
