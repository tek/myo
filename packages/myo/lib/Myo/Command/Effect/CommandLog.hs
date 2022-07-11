module Myo.Command.Effect.CommandLog where

import Chiasma.Data.Ident (Ident)
import qualified Data.Text as Text
import Prelude hiding (get)

data CommandLog :: Effect where
  Set :: Ident -> Text -> CommandLog m ()
  Append :: Ident -> ByteString -> CommandLog m ()
  Archive :: Ident -> CommandLog m ()
  ArchiveAll :: CommandLog m ()
  Get :: Ident -> CommandLog m (Maybe Text)
  GetPrev :: Ident -> CommandLog m (Maybe Text)
  All :: CommandLog m (Map Ident Text)

makeSem ''CommandLog

getLines ::
  Member CommandLog r =>
  Ident ->
  Sem r (Maybe [Text])
getLines i =
  fmap Text.lines <$> get i