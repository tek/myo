module Myo.Test.Output.ParseTest where

import qualified Data.Vector as Vector
import Path (relfile)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, evalMaybe, (/==))
import Ribosome (interpretPersistNull)
import Ribosome.Test (assertWait, testError, testHandler)

import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Interpreter.Backend.Process (interpretBackendProcessNative)
import Myo.Command.Interpreter.CommandLog (interpretCommandLogSetting)
import Myo.Command.Parse (parseCommand, selectCommand)
import Myo.Command.Run (myoRunIdent)
import Myo.Interpreter.Controller (interpretController)
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
  myoTest $ interpretPersistNull $ interpretCommandLogSetting $ interpretBackendProcessNative $ interpretController do
    file <- Test.fixturePath [relfile|tmux/parse/file|]
    interpretParsing [(echoLang, [parseEcho file])] $ testHandler do
        ident <- addEchoCommand "proc" lines' False
        myoRunIdent ident
        assertWait (CommandLog.get ident) evalMaybe
        cmd <- testError (selectCommand (Just ident))
        CommandLog.archive ident
        CommandLog.append ident "unparsable"
        outputEvents <- testError (fmap (fmap ParsedOutput.events) <$> parseCommand cmd)
        let ParseReport events _ = compileReport 0 (foldMap fold outputEvents)
        Vector.empty /== events
