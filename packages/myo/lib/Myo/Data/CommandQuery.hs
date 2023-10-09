module Myo.Data.CommandQuery where

import Exon (exon)

import Myo.Data.CommandId (CommandId, commandIdText)
import Myo.Data.CommandName (CommandName)

data CommandQueryField =
  CommandById CommandId
  |
  CommandByName CommandName
  |
  CommandByAny Text
  |
  CommandByIndex Int
  deriving stock (Eq, Show, Generic)

data CommandQueryDomain =
  QueryCommands
  |
  QueryHistory
  |
  QueryBoth
  deriving stock (Eq, Show, Generic)

data CommandQuery =
  CommandQuery {
    field :: CommandQueryField,
    domain :: CommandQueryDomain
  }
  deriving stock (Eq, Show, Generic)

queryId ::
  CommandId ->
  CommandQuery
queryId i =
  CommandQuery (CommandById i) QueryCommands

queryIdH ::
  CommandId ->
  CommandQuery
queryIdH i =
  CommandQuery (CommandById i) QueryHistory

queryIdBoth ::
  CommandId ->
  CommandQuery
queryIdBoth i =
  CommandQuery (CommandById i) QueryBoth

queryName ::
  CommandName ->
  CommandQuery
queryName name =
  CommandQuery (CommandByName name) QueryCommands

queryNameH ::
  CommandName ->
  CommandQuery
queryNameH name =
  CommandQuery (CommandByName name) QueryHistory

queryNameBoth ::
  CommandName ->
  CommandQuery
queryNameBoth name =
  CommandQuery (CommandByName name) QueryBoth

queryIndex ::
  Int ->
  CommandQuery
queryIndex index =
  CommandQuery (CommandByIndex index) QueryHistory

queryAny ::
  Text ->
  CommandQuery
queryAny name =
  CommandQuery (CommandByAny name) QueryCommands

describe :: CommandQuery -> Text
describe query =
  [exon|#{describeField query.field} in #{describeDomain query.domain}|]
  where
    describeField = \case
      CommandById i -> [exon|ident '#{commandIdText i}'|]
      CommandByName name -> [exon|display name '##{name}'|]
      CommandByAny name -> [exon|ident or name '#{name}'|]
      CommandByIndex index -> [exon|index #{show index}|]

    describeDomain = \case
      QueryCommands -> "commands"
      QueryHistory -> "history"
      QueryBoth -> "commands or history"
