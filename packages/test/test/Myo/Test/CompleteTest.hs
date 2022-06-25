module Myo.Test.CompleteTest where

import Polysemy.Test (UnitTest, (===))

import Myo.Command.Data.Command (Command (Command))
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter
import Myo.Command.Data.CommandState (CommandState)
import Myo.Complete (myoCompleteCommand)
import Myo.Test.Run (myoTest)

commands :: [Command]
commands =
  [cmd "cmd-15", cmd "cmd-20", cmd "cmd-16"]
  where
    cmd n =
      Command (CommandInterpreter.System def) n [] def def def False False False

test_completeCommand :: UnitTest
test_completeCommand =
  myoTest do
    atomicSet @CommandState #commands commands
    result <- myoCompleteCommand "cmd-1"
    ["cmd-15", "cmd-16"] === result
