module Myo.Test.RunTest where

import Chiasma.Data.Ident (Ident (Str))
import qualified Data.Map.Strict as Map
import Log (Severity (Error))
import Polysemy.Test (Hedgehog, UnitTest, assertJust, evalMaybe, (===))
import Ribosome (
  ErrorMessage (ErrorMessage),
  Errors,
  HandlerTag,
  HostError,
  StoredError (StoredError),
  ToErrorMessage (toErrorMessage),
  reportError,
  )
import qualified Ribosome.Errors as Errors
import Ribosome.Test (testHandler)

import Myo.Command.Add (myoAddSystemCommand)
import qualified Myo.Command.Data.AddSystemCommandOptions as AddSystemCommandOptions
import Myo.Command.Data.AddSystemCommandOptions (runner)
import Myo.Command.Data.Command (Command (Command), lines)
import Myo.Command.Data.RunLineOptions (line, runner)
import Myo.Command.Data.RunTask (RunTask (RunTask), command)
import Myo.Command.Run (myoLine, myoRunIdent)
import Myo.Data.ProcessCommand (ProcessCommand)
import qualified Myo.Effect.Executor as Executor
import Myo.Effect.Executor (Executor)
import Myo.Interpreter.Controller (interpretController)
import Myo.Interpreter.Executor.Process (interpretExecutorProcessNative)
import Myo.Test.Run (myoTest)

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

interpretExecutorDummy ::
  Member (DataLog HostError) r =>
  InterpreterFor (Executor ()) r
interpretExecutorDummy =
  interpret \case
    Executor.Accept _ ->
      pure (Just ())
    Executor.Run _ ->
      Nothing <$ reportError (Just htag) (RunTestError [testError])

test_runSystem :: UnitTest
test_runSystem =
  myoTest $ interpretExecutorDummy $ interpretController @'[()] do
    testHandler do
      myoAddSystemCommand (AddSystemCommandOptions.cons ident ["ls"]) { runner = Just runnerIdent }
      myoRunIdent ident
    checkReport [testError]

cmdline :: Text
cmdline = "echo 'hello'"

interpretExecutorDummySingleLine ::
  Member (DataLog HostError) r =>
  InterpreterFor (Executor [Text]) r
interpretExecutorDummySingleLine =
  interpret \case
    Executor.Accept RunTask {command = Command {lines = l}} ->
      pure (Just l)
    Executor.Run l ->
      Nothing <$ reportError (Just htag) (RunTestError l)

test_runLineSingle :: UnitTest
test_runLineSingle =
  myoTest $ interpretExecutorDummy $ interpretController @'[()] do
    testHandler do
      myoAddSystemCommand (AddSystemCommandOptions.cons ident ["ls"]) { runner = Just runnerIdent }
      myoLine def { line = Just cmdline, runner = Just runnerIdent }
    checkReport [cmdline]

test_runSubproc :: UnitTest
test_runSubproc =
  myoTest $ interpretExecutorProcessNative $ interpretController @'[ProcessCommand] do
    testHandler do
      myoAddSystemCommand (AddSystemCommandOptions.cons ident ["ls -234234"]) { runner = Just runnerIdent }
      myoRunIdent ident
      myoRunIdent ident
    loggedError <- evalMaybe . Map.lookup "command" =<< Errors.get
    2 === length loggedError
