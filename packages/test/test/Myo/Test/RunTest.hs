module Myo.Test.RunTest where

import Log (Severity (Warn))
import Polysemy.Test (UnitTest, assertLeft)
import Ribosome (Report (Report))
import Ribosome.Api (defineFunction, nvimSetVar)
import Ribosome.Test (testHandler)

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions, systemOptions)
import Myo.Command.Data.CommandSpec (parseCommandSpec')
import Myo.Command.Data.Param (ParamDefault (ParamDefault), ParamId, ParamValue (ParamFlag))
import Myo.Command.Data.RunLineOptions (line)
import Myo.Command.Interpreter.Backend.Process (interpretBackendProcessNative)
import Myo.Command.Run (myoLine, runIdent)
import Myo.Data.CommandId (CommandId)
import Myo.Interpreter.Controller (interpretControllerTransient)
import Myo.Test.Backend (checkReport, interpretBackendDummy, interpretBackendDummySingleLine, testError)
import Myo.Test.Embed (myoTest)

ident :: CommandId
ident =
  "cmd"

test_runSystem :: UnitTest
test_runSystem =
  myoTest $ interpretBackendDummy $ interpretControllerTransient [] do
    testHandler do
      myoAddSystemCommand (systemOptions ident ["ls"])
      runIdent ident mempty
    checkReport [testError]

cmdline :: Text
cmdline = "echo 'hello'"

test_runLineSingle :: UnitTest
test_runLineSingle =
  myoTest $ interpretBackendDummySingleLine $ interpretControllerTransient [] do
    testHandler do
      myoAddSystemCommand (systemOptions ident ["ls"])
      myoLine def { line = Just (parseCommandSpec' [cmdline]) }
    checkReport [cmdline]

test_runSubprocFail :: UnitTest
test_runSubprocFail =
  myoTest $ interpretBackendProcessNative $ interpretControllerTransient [] do
    testHandler do
      myoAddSystemCommand (systemOptions ident ["ls -234234"])
      _ <- assertLeft err =<< runStop @Report (runIdent ident mempty)
      unit
  where
    err = Report "subprocess failed: exit code 2" ["RunError.SubprocFailed"] Warn

paramDefaults :: Map ParamId ParamDefault
paramDefaults =
  [("par2", "default value 2"), ("par3", "default value 3"), ("par5", ParamDefault (ParamFlag False))]

paramCommand :: AddSystemCommandOptions
paramCommand =
  systemOptions ident [line]
  & #params ?~ paramDefaults
  where
    line = "cmd: {par1} / {par2:sub ({par1}) ({par2})} / {par3} / {par4:}{par5?bool value 1}{par6?bool value 2}"

test_runParamCommand :: UnitTest
test_runParamCommand =
  myoTest $ interpretBackendDummySingleLine $ interpretControllerTransient [] do
    testHandler do
      myoAddSystemCommand paramCommand
      nvimSetVar @Text "myo_param_par1" "var value 1"
      nvimSetVar "myo_param_par6" True
      defineFunction "Myo_param_par2" [] ["return 'fun value 2'"]
      runIdent ident mempty
    checkReport ["cmd: var value 1 / sub (var value 1) (fun value 2) / default value 3 / bool value 2"]
