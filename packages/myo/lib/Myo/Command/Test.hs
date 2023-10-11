module Myo.Command.Test where

import Ribosome (Handler, Rpc, RpcError, mapReport, resumeReports)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandInterpreter (CommandInterpreter (System))
import qualified Myo.Command.Data.CommandSpec
import qualified Myo.Command.Data.CommandTemplate
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Override (queryOverrides)
import Myo.Data.CommandId (CommandId)
import Myo.Data.CommandQuery (queryId)
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)
import qualified Myo.Effect.Controller as Controller
import Myo.Effect.Controller (Controller)

testCommandCallback :: Text
testCommandCallback = "MyoTestOverrides"

testCommandId :: CommandId
testCommandId = "myo-test"

defaultBaseCommand :: Command
defaultBaseCommand =
  Command.cons (System (Just "make")) testCommandId []

baseCommand ::
  Member Commands r =>
  Sem r Command
baseCommand =
  fromMaybeA (defaultBaseCommand <$ Commands.add defaultBaseCommand) =<< Commands.lookup (queryId testCommandId)

emptyTestCmdline :: Text
emptyTestCmdline =
  "The test command line is empty." <>
  " Either set a default for the command 'myo-test' or return it from 'MyoTestOverrides()'."

runTest ::
  Members [Controller, Commands, Rpc, Stop CommandError] r =>
  Sem r ()
runTest = do
  (cmd, params) <- queryOverrides baseCommand testCommandCallback
  case cmd.cmdLines.template.rendered of
    [] -> stop (CommandError.User emptyTestCmdline)
    _ -> Controller.runCommand cmd params

myoTest ::
  Members [Controller !! RunError, Commands !! CommandError, Rpc !! RpcError] r =>
  Handler r ()
myoTest = do
  resumeReports @[Controller, Commands, Rpc] @[_, _, _] $ mapReport @CommandError runTest
