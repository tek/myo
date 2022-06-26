module Myo.Effect.CommandLog where

import Chiasma.Data.Ident (Ident)

data CommandLog :: Effect where
  Start :: Ident -> CommandLog m ()
  Append :: Ident -> Text -> CommandLog m ()
  Get :: Ident -> CommandLog m (Maybe Text)

makeSem ''CommandLog
