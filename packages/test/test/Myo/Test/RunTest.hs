module Myo.Test.RunTest where

import Chiasma.Data.Ident (Ident (Str))
import qualified Data.Map.Strict as Map
import Log (Severity (Error))
import Polysemy.Test (Hedgehog, UnitTest, assertJust, evalLeft)
import Ribosome (
  ErrorMessage (ErrorMessage),
  Errors,
  HandlerTag,
  HostError,
  StoredError (StoredError),
  ToErrorMessage (toErrorMessage),
  interpretPersistNull,
  reportError,
  )
import qualified Ribosome.Errors as Errors
import Ribosome.Test (testHandler)

import Myo.Command.Add (myoAddSystemCommand)
import qualified Myo.Command.Data.AddSystemCommandOptions as AddSystemCommandOptions
import Myo.Command.Data.AddSystemCommandOptions (runner)
import Myo.Command.Data.Command (Command (Command, cmdLines))
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunLineOptions (line, runner)
import Myo.Command.Data.RunTask (RunTask (RunTask), command)
import Myo.Command.Effect.Backend (Backend)
import Myo.Command.Interpreter.Backend.Generic (captureUnsupported, interceptBackend, interpretBackendFail)
import Myo.Command.Interpreter.Backend.Process (interpretBackendProcessNative)
import Myo.Command.Interpreter.CommandLog (interpretCommandLogSetting)
import Myo.Command.Run (myoLine, myoRunIdent)
import qualified Myo.Effect.Controller as Controller
import Myo.Interpreter.Controller (interpretController)
import Myo.Test.Embed (myoTest)

newtype RunTestError =
  RunTestError { unTestError :: [Text] }
  deriving stock (Eq, Show)
  deriving newtype (Ord)

instance ToErrorMessage RunTestError where
  toErrorMessage (RunTestError e) =
    ErrorMessage (unlines e) e Error

testError :: Text
testError =
  "error"

htag :: HandlerTag
htag = "test"

runnerIdent :: Ident
runnerIdent =
  Str "dummy"

ident :: Ident
ident =
  Str "cmd"

checkReport ::
  Members [Hedgehog IO, Errors] r =>
  [Text] ->
  Sem r ()
checkReport target = do
  loggedError <- Map.lookup htag <$> Errors.get
  assertJust [target] (fmap user <$> loggedError)
  where
    user (StoredError (ErrorMessage _ l _) _) =
      l

dummyAccept :: RunTask -> Sem r (Maybe ())
dummyAccept _ =
  pure (Just ())

dummyExecute ::
  Member (DataLog HostError) r =>
  () ->
  Sem r ()
dummyExecute () =
  reportError (Just htag) (RunTestError [testError])

interpretBackendDummy ::
  Member (DataLog HostError) r =>
  InterpreterFor (Backend !! RunError) r
interpretBackendDummy =
  interpretBackendFail .
  interceptBackend dummyAccept dummyExecute (captureUnsupported "dummy") unit

test_runSystem :: UnitTest
test_runSystem =
  myoTest $ interpretCommandLogSetting $ interpretPersistNull $ interpretBackendDummy $ interpretController do
    testHandler do
      myoAddSystemCommand (AddSystemCommandOptions.cons ident ["ls"]) { runner = Just runnerIdent }
      myoRunIdent ident
    checkReport [testError]

cmdline :: Text
cmdline = "echo 'hello'"

singleLineAccept :: RunTask -> Sem r (Maybe [Text])
singleLineAccept RunTask {command = Command {cmdLines = l}} =
  pure (Just l)

singleLineExecute ::
  Member (DataLog HostError) r =>
  [Text] ->
  Sem r ()
singleLineExecute l =
  reportError (Just htag) (RunTestError l)

interpretBackendDummySingleLine ::
  Member (DataLog HostError) r =>
  InterpreterFor (Backend !! RunError) r
interpretBackendDummySingleLine =
  interpretBackendFail .
  interceptBackend singleLineAccept singleLineExecute (captureUnsupported "dummy") unit

test_runLineSingle :: UnitTest
test_runLineSingle =
  myoTest $ interpretCommandLogSetting $ interpretPersistNull $ interpretBackendDummySingleLine $ interpretController do
    testHandler do
      myoAddSystemCommand (AddSystemCommandOptions.cons ident ["ls"]) { runner = Just runnerIdent }
      myoLine def { line = Just cmdline, runner = Just runnerIdent }
    checkReport [cmdline]

test_runSubprocFail :: UnitTest
test_runSubprocFail =
  myoTest $ interpretCommandLogSetting $ interpretPersistNull $ interpretBackendProcessNative $ interpretController do
    r <- testHandler do
      myoAddSystemCommand (AddSystemCommandOptions.cons ident ["ls -234234"]) { runner = Just runnerIdent }
      resumeEither (Controller.runIdent ident)
    _ <- evalLeft r
    unit
