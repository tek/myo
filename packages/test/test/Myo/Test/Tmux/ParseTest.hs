module Myo.Test.Tmux.ParseTest where

import Conc (interpretSync)
import qualified Control.Lens as Lens
import Exon (exon)
import Hedgehog.Internal.Property (Failure)
import Path (relfile)
import Polysemy.Chronos (ChronosTime)
import qualified Polysemy.Test as Test
import Polysemy.Test (Hedgehog, TestError, UnitTest, assert, assertJust, evalMaybe)
import Ribosome.Api (currentBufferContent, currentLine)
import Ribosome.Test (assertWait, resumeTestError, testHandler)

import Myo.Command.Data.ParseOptions (ParseOptions (ParseOptions))
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import Myo.Command.Interpreter.Backend.Tmux (interpretBackendTmuxWithLog)
import Myo.Command.Interpreter.SocketReader (interpretSocketReader)
import Myo.Command.Parse (myoParse)
import Myo.Command.Run (runIdent)
import Myo.Data.CommandId (CommandId)
import qualified Myo.Effect.Controller as Controller
import Myo.Effect.Controller (Controller)
import Myo.Interpreter.Controller (interpretControllerTransient)
import Myo.Interpreter.Outputs (interpretOutputs)
import Myo.Output.Data.OutputParser (OutputParser (OutputParser))
import Myo.Output.Interpreter.Parsing (interpretParsing)
import Myo.Test.Embed (myoEmbedTmuxTest)
import Myo.Test.Output.Echo (addEchoCommand, echoLang, parseEcho)
import Myo.Ui.Default (setupDefaultTestUi)

line1 :: Text
line1 = "line 1"

line2 :: Text
line2 = "line 2"

waitForLog ::
  Member (Controller !! RunError) r =>
  Members [CommandLog, Hedgehog IO, Error Failure, Error TestError, ChronosTime, Race, Embed IO] r =>
  CommandId ->
  Sem r ()
waitForLog i =
  assertWait (resumeTestError (Controller.captureOutput i) *> CommandLog.getLines i) \ mayLog -> do
    log <- evalMaybe mayLog
    assert ([exon|echoline #{line2}|] `elem` log)

test_parseTmux :: UnitTest
test_parseTmux =
  myoEmbedTmuxTest $ interpretSocketReader $ interpretBackendTmuxWithLog $ interpretSync $
  interpretOutputs $ interpretControllerTransient [] do
    setupDefaultTestUi
    file <- Test.fixturePath [relfile|tmux/parse/file|]
    interpretParsing [(echoLang, [OutputParser (parseEcho file)])] $ testHandler do
      ident <- addEchoCommand [line1, line2] True
      runIdent ident mempty
      waitForLog ident
      myoParse (ParseOptions Nothing Nothing Nothing)
      index <- currentLine
      assertJust "line 1" . Lens.firstOf (Lens.element index) =<< currentBufferContent

test_parseCaptureTmux :: UnitTest
test_parseCaptureTmux =
  myoEmbedTmuxTest $ interpretSocketReader $ interpretBackendTmuxWithLog $ interpretSync $
  interpretOutputs $ interpretControllerTransient [] do
    setupDefaultTestUi
    file <- Test.fixturePath [relfile|tmux/parse/file|]
    interpretParsing [(echoLang, [OutputParser (parseEcho file)])] $ testHandler do
      ident <- addEchoCommand [line1, line2] True
      runIdent ident mempty
      waitForLog ident
      myoParse (ParseOptions Nothing Nothing Nothing)
      index <- currentLine
      assertJust "line 1" . Lens.firstOf (Lens.element index) =<< currentBufferContent
