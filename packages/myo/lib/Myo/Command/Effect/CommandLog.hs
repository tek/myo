module Myo.Command.Effect.CommandLog where

import qualified Data.Text as Text
import Prelude hiding (get)

import Myo.Data.CommandId (CommandId)

data CommandLog :: Effect where
  Set :: CommandId -> Text -> CommandLog m ()
  Append :: CommandId -> ByteString -> CommandLog m ()
  Archive :: CommandId -> CommandLog m ()
  ArchiveAll :: CommandLog m ()
  Get :: CommandId -> CommandLog m (Maybe Text)
  GetPrev :: CommandId -> CommandLog m (Maybe Text)
  All :: CommandLog m (Map CommandId Text)

makeSem ''CommandLog

getLines ::
  Member CommandLog r =>
  CommandId ->
  Sem r (Maybe [Text])
getLines i =
  fmap Text.lines <$> get i
