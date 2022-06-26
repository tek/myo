module Myo.Test.Tmux.ParseTest where

import qualified Control.Lens as Lens
import Path (relfile)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertJust, evalMaybe)
import Ribosome.Api (currentBufferContent, currentLine)
import Ribosome.Test (assertWait, testHandler)

import qualified Myo.Command.Data.CommandLog as CommandLog
import Myo.Command.Data.CommandLog (CommandLog)
import Myo.Command.Data.ParseOptions (ParseOptions (ParseOptions))
import Myo.Command.Log (commandLog)
import Myo.Command.Run (myoRunIdent)
import Myo.Output.Interpreter.CommandOutput (interpretCommandOutput)
import Myo.Test.Output.Echo (addEchoCommand, echoLang, parseEcho)
import Myo.Test.Run (myoEmbedTmuxTest)
import Myo.Test.Tmux.Output (containsLine)

line1 :: Text
line1 = "line 1"

line2 :: Text
line2 = "line 2"

checkLog ::
  Sem r (Maybe CommandLog) ->
  Sem r ()
checkLog getLog =
  assertWait (fmap (lines . decodeUtf8 . CommandLog.current) <$> getLog) \ mayLog -> do
    log <- evalMaybe mayLog
    containsLine ("echoline " <> line2) (decodeUtf8 <$> log)

-- parseTmuxTest :: Sem r ()
-- parseTmuxTest = do
--   lift initialize''
--   lift . addEchoHandler =<< fixture "tmux/parse/file"
--   ident <- lift (addEchoCommand "tmux" [line1, line2] False)
--   lift (myoRunIdent ident)
--   await logComplete (fmap (lines . decodeUtf8 . CommandLog.current) <$> commandLog ident)
--   myoParse $ ParseOptions Nothing Nothing Nothing
--   index <- currentLine
--   (Just "line 1" ===) =<< Lens.firstOf (Lens.element index) <$> currentBufferContent

-- test_parseTmux :: UnitTest
-- test_parseTmux =
--   tmuxTestDef parseTmuxTest

test_parseCaptureTmux :: UnitTest
test_parseCaptureTmux =
  myoEmbedTmuxTest do
    file <- Test.fixturePath [relfile|tmux/parse/file|]
    interpretCommandOutput [(echoLang, parseEcho file)] $ testHandler do
      ident <- addEchoCommand "tmux" [line1, line2] True
      myoRunIdent ident
      checkLog (logOrCaptureByIdent ident True)
      myoParse $ ParseOptions Nothing Nothing Nothing
      index <- currentLine
      assertJust "line 1" =<< Lens.firstOf (Lens.element index) <$> currentBufferContent
    where
      logComplete mayLog = do
        log' <- evalMaybe mayLog
        containsLine ("echoline " <> line2) (decodeUtf8 <$> log')
