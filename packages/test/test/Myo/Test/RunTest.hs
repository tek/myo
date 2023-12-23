module Myo.Test.RunTest where

import qualified Conc
import Exon (exon)
import Log (Severity (Warn))
import Polysemy.Test (UnitTest, assertLeft)
import Ribosome (Execution (Sync), Report (Report), rpcCommand)
import Ribosome.Api (defineFunction, nvimCommand, nvimSetVar)
import Ribosome.Test (testHandler)
import Time (Seconds (Seconds))

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions, systemOptions)
import Myo.Command.Data.CommandSpec (parseCommandSpec')
import Myo.Command.Data.Param (ParamDefault (ParamDefault), ParamId, ParamValue (ParamFlag))
import Myo.Command.Data.RunLineOptions (line)
import Myo.Command.Interpreter.Backend.Process (interpretBackendProcessNative)
import Myo.Command.Run (myoLine, myoRun, runIdent)
import Myo.Data.CommandId (CommandId, commandIdText)
import Myo.Interpreter.Controller (interpretControllerTransient)
import Myo.Test.Backend (checkReport, interpretBackendDummy, interpretBackendDummySingleLine, testError)
import Myo.Test.Embed (myoTest)
import Myo.Test.Handler (myoTestHandlers)

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
  [
    ("par2", "default value 2"),
    ("par3", "default value 3"),
    ("par5", ParamDefault (ParamFlag False)),
    ("par8", "default value 8")
  ]

paramCommand :: AddSystemCommandOptions
paramCommand =
  systemOptions ident [line]
  & #params ?~ paramDefaults
  where
    line =
      "cmd: {par1} / {par2:sub ({par1}) ({par2})} / {par3} / {par4:}{par5? / bool value 1}{par6?" <>
      " / bool value 2}{par7:optional {par7}} / {par8}"

test_runParamCommand :: UnitTest
test_runParamCommand =
  myoTest $ interpretBackendDummySingleLine $ interpretControllerTransient [] do
    testHandler do
      myoAddSystemCommand paramCommand
      nvimSetVar @Text "myo_param_par1" "var value 1"
      nvimSetVar "myo_param_par6" True
      defineFunction "Myo_param_par2" [] ["return 'fun value 2'"]
      defineFunction "Myo_param_par8" [] ["return { 'ignore': 1 }"]
      runIdent ident mempty
    checkReport ["cmd: var value 1 / sub (var value 1) (fun value 2) / default value 3 /  / bool value 2 / default value 8"]

test_runParamCommandOptparse :: UnitTest
test_runParamCommandOptparse = do
  let
    extra = interpretBackendDummySingleLine . interpretControllerTransient []
    handlers = [rpcCommand "MyoRun" Sync myoRun]
  myoTestHandlers @[_, _, _] extra handlers do
    testHandler do
      myoAddSystemCommand paramCommand
      nvimSetVar @Text "myo_param_par1" "var value 1"
      nvimSetVar "myo_param_par6" True
      defineFunction "Myo_param_par2" [] ["return 'fun value 2'"]
      Conc.timeout_ (throw "MyoRun command timed out") (Seconds 3) do
        nvimCommand [exon|MyoRun #{commandIdText ident} --par1=par1-opt --par2='par2 " opt' --par4="par4 ' op"t --par5|]
    checkReport [[exon|cmd: par1-opt / sub (par1-opt) (par2 " opt) / default value 3 / par4 ' opt / bool value 1 / bool value 2|]]
