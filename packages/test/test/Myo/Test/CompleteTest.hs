module Myo.Test.CompleteTest where

import Polysemy.Test (UnitTest, (===))
import Ribosome.Test (resumeTestError)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command)
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter
import Myo.Complete (myoCompleteCommand)
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)
import Myo.Interpreter.Commands (interpretCommandsTransient)
import Myo.Test.Embed (myoTest)

commands :: [Command]
commands =
  [cmd "cmd-15", cmd "cmd-20", cmd "cmd-16"]
  where
    cmd n =
      Command.cons (CommandInterpreter.System def) n []

test_completeCommand :: UnitTest
test_completeCommand =
  myoTest $ interpretCommandsTransient [] do
    resumeTestError @Commands (traverse_ Commands.add commands)
    result <- myoCompleteCommand "cmd-1" "" 0
    ["cmd-15", "cmd-16"] === result
