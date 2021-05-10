module Myo.Test.CompleteTest where

import Hedgehog ((===))
import Ribosome.Test.Run (UnitTest)

import Myo.Command.Data.Command (Command(Command))
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter
import qualified Myo.Command.Data.CommandState as CommandState
import Myo.Command.Data.CommandState (CommandState)
import Myo.Complete (completeCommand)
import Myo.Test.Unit (MyoTest, testDef)

commands :: [Command]
commands =
  [cmd "cmd-15", cmd "cmd-20", cmd "cmd-16"]
  where
    cmd n =
      Command (CommandInterpreter.System def) n [] def def def False False False

completeCommandTest :: MyoTest ()
completeCommandTest = do
  setL @CommandState CommandState.commands commands
  result <- completeCommand "cmd-1"
  ["cmd-15", "cmd-16"] === result

test_completeCommand :: UnitTest
test_completeCommand =
  testDef completeCommandTest
