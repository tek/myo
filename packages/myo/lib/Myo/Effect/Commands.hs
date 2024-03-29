module Myo.Effect.Commands where

import Myo.Command.Data.Command (Command)
import Myo.Data.CommandId (CommandId)
import Myo.Data.CommandName (CommandName)
import qualified Myo.Data.CommandQuery as CommandQuery
import Myo.Data.CommandQuery (CommandQuery)

data Commands :: Effect where
  All :: Commands m [Command]
  Latest :: Commands m Command
  Query :: CommandQuery -> Commands m Command
  Lookup :: CommandQuery -> Commands m (Maybe Command)
  Add :: Command -> Commands m ()

makeSem ''Commands

queryId ::
  Member Commands r =>
  CommandId ->
  Sem r Command
queryId =
  query . CommandQuery.queryId

queryIdBoth ::
  Member Commands r =>
  CommandId ->
  Sem r Command
queryIdBoth =
  query . CommandQuery.queryIdBoth

queryName ::
  Member Commands r =>
  CommandName ->
  Sem r Command
queryName =
  query . CommandQuery.queryName

queryNameBoth ::
  Member Commands r =>
  CommandName ->
  Sem r Command
queryNameBoth =
  query . CommandQuery.queryNameBoth
