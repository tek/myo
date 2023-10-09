module Myo.Effect.History where

import Myo.Command.Data.Command (Command)
import qualified Myo.Command.Data.HistoryEntry
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Data.Param (ParamValues)
import Myo.Data.CommandId (CommandId)
import qualified Myo.Data.CommandQuery as CommandQuery
import Myo.Data.CommandQuery (CommandQuery, CommandQueryDomain (QueryHistory))

data History :: Effect where
  All :: History m [HistoryEntry]
  Latest :: History m HistoryEntry
  Push :: Command -> CommandId -> ParamValues -> [Text] -> History m ()
  Remove :: NonEmpty CommandId -> History m ()
  Load :: History m ()
  Query :: CommandQuery -> History m HistoryEntry

makeSem ''History

queryId ::
  Member History r =>
  CommandId ->
  Sem r HistoryEntry
queryId i =
  query (CommandQuery.queryId i & #domain .~ QueryHistory)

queryCommand ::
  Member History r =>
  CommandQuery ->
  Sem r Command
queryCommand =
  fmap (.command) . query
