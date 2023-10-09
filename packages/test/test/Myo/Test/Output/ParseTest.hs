module Myo.Test.Output.ParseTest where

import qualified Data.Vector as Vector
import Path (relfile)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, evalMaybe, (/==))
import Ribosome.Test (assertWait, resumeTestError, testError, testHandler)

import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Interpreter.Backend.Process (interpretBackendProcessNative)
import Myo.Command.Parse (parseCommand, selectCommand)
import Myo.Command.Run (runIdent)
import Myo.Effect.Commands (Commands)
import Myo.Interpreter.Controller (interpretControllerTransient)
import Myo.Output.Data.ParseReport (ParseReport (ParseReport))
import qualified Myo.Output.Data.ParsedOutput as ParsedOutput (events)
import Myo.Output.Interpreter.Parsing (interpretParsing)
import Myo.Output.ParseReport (compileReport)
import Myo.Test.Embed (myoTest)
import Myo.Test.Output.Echo (addEchoCommand, echoLang, parseEcho)

lines' :: [Text]
lines' =
  ["line1"]

test_parsePrevious :: UnitTest
test_parsePrevious =
  myoTest $ interpretBackendProcessNative $ interpretControllerTransient [] do
    file <- Test.fixturePath [relfile|tmux/parse/file|]
    interpretParsing [(echoLang, [parseEcho file])] $ testHandler do
        ident <- addEchoCommand "proc" lines' False
        runIdent ident mempty
        assertWait (CommandLog.get ident) evalMaybe
        cmd <- resumeTestError @Commands (selectCommand (Just ident))
        CommandLog.archive ident
        CommandLog.append ident "unparsable"
        outputEvents <- testError (fmap (fmap (.events)) <$> parseCommand cmd)
        let ParseReport events _ = compileReport 0 (foldMap fold outputEvents)
        Vector.empty /== events
