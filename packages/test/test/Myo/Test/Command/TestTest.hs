module Myo.Test.Command.TestTest where

import Exon (exon)
import Polysemy.Test (TestError, UnitTest, assertLeft)
import Ribosome (Rpc, RpcError)
import Ribosome.Api (defineFunction, nvimSetVar)
import Ribosome.Test (resumeTestError, testError, testHandler)

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (systemOptions)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Test (emptyTestCmdline, runTest, testCommandId)
import Myo.Effect.Commands (Commands)
import Myo.Effect.Controller (Controller)
import Myo.Interpreter.Controller (interpretControllerTransient)
import Myo.Test.Backend (checkReport, interpretBackendDummySingleLine)
import Myo.Test.Embed (myoTest)

line :: Text
line = "cmd: {par1} / {par2:sub ({par1}) ({par2})} / {par3}{par4:}{par5? / bool value 1}{par6? / bool value 2}"

testCommandTest ::
  Members [Controller !! RunError, Commands !! CommandError, Rpc !! RpcError, Rpc, Error TestError] r =>
  Sem r ()
testCommandTest = do
  nvimSetVar @Text "myo_param_par1" "var value 1"
  nvimSetVar "myo_param_par6" True
  defineFunction "Myo_param_par2" [] ["return 'fun value 2'"]
  defineFunction "MyoTestOverrides" ["meta"] [[exon|return { 'lines': '#{line}', 'params': { #{testParams} } }|]]
  resumeTestError @Controller $ resumeTestError @Commands $ testError runTest
  where
    testParams = [exon|'par3': 'test-par-3', 'par5': v:true |]

test_testCommand :: UnitTest
test_testCommand =
  myoTest $ interpretBackendDummySingleLine $ interpretControllerTransient [] do
    testHandler testCommandTest
    checkReport ["cmd: var value 1 / sub (var value 1) (fun value 2) / test-par-3 / bool value 1 / bool value 2"]

test_testCommandDefault :: UnitTest
test_testCommandDefault =
  myoTest $ interpretBackendDummySingleLine $ interpretControllerTransient [] do
    testHandler $ myoAddSystemCommand (systemOptions testCommandId [""] & #params ?~ defs)
    testHandler testCommandTest
    checkReport [
      "cmd: var value 1 / sub (var value 1) (fun value 2) / test-par-3 / default value 4 / bool value 1 / bool value 2"
      ]
  where
    defs = [("par3", "default value 3"), ("par4", " / default value 4")]

test_testCommandEmpty :: UnitTest
test_testCommandEmpty =
  myoTest $ interpretBackendDummySingleLine $ interpretControllerTransient [] do
    result <- resumeTestError @Controller $ resumeTestError @Commands $ runStop runTest
    assertLeft (CommandError.User emptyTestCmdline) result
