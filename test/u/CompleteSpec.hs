{-# OPTIONS_GHC -F -pgmF htfpp #-}

module CompleteSpec (htf_thisModulesTests) where

import Test.Framework

import Myo.Command.Data.Command (Command(Command))
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState
import Myo.Complete (completeCommand)
import Myo.Data.Env (Myo)
import Unit (specDef)

commands :: [Command]
commands =
  [cmd "cmd-15", cmd "cmd-20", cmd "cmd-16"]
  where
    cmd n =
      Command (CommandInterpreter.System def) n [] def def def False False False

completeCommandSpec :: Myo ()
completeCommandSpec = do
  setL @CommandState CommandState.commands commands
  result <- completeCommand "cmd-1"
  gassertEqual ["cmd-15", "cmd-16"] result

test_completeCommand :: IO ()
test_completeCommand =
  specDef completeCommandSpec
