module Myo.Test.CompleteTest where

import Polysemy.Test (UnitTest, (===))

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command)
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter
import Myo.Command.Data.CommandState (CommandState)
import Myo.Complete (myoCompleteCommand)
import Myo.Test.Embed (myoTest)

commands :: [Command]
commands =
  [cmd "cmd-15", cmd "cmd-20", cmd "cmd-16"]
  where
    cmd n =
      Command.cons (CommandInterpreter.System def) n []

test_completeCommand :: UnitTest
test_completeCommand =
  myoTest do
    atomicSet @CommandState #commands commands
    result <- myoCompleteCommand "cmd-1"
    ["cmd-15", "cmd-16"] === result
