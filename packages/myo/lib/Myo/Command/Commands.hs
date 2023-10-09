module Myo.Command.Commands where

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command, commandHasAny, commandHasId, commandHasName)
import Myo.Data.CommandQuery (CommandQuery, CommandQueryField (..))
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)

describe ::
  Member Commands r =>
  CommandQuery ->
  Sem r Text
describe query = do
  cmd <- Commands.query query
  pure (Command.describe cmd)

matcher :: (a -> Command) -> CommandQueryField -> [a] -> Maybe a
matcher lens = \case
  CommandById i -> find (commandHasId i . lens)
  CommandByName name -> find (commandHasName name . lens)
  CommandByAny name -> find (commandHasAny name . lens)
  CommandByIndex index -> head . drop index
