module Myo.Test.Tmux.ParseTest where

import qualified Control.Lens as Lens (element, firstOf)
import qualified Data.ByteString.Char8 as ByteString (lines)
import Hedgehog (evalMaybe, (===))
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Api.Window (currentLine)
import Ribosome.Test.Await (await)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (fixture)

import qualified Myo.Command.Data.CommandLog as CommandLog (_current)
import Myo.Command.Data.ParseOptions (ParseOptions(ParseOptions))
import Myo.Command.Log (commandLog)
import Myo.Command.Parse (logOrCaptureByIdent, myoParse)
import Myo.Command.Run (myoRunIdent)
import Myo.Init (initialize'')
import Myo.Test.Output.Echo (addEchoCommand, addEchoHandler)
import Myo.Test.Tmux.Output (containsLine)
import Myo.Test.Unit (MyoTest, tmuxTestDef)

line1 :: Text
line1 = "line 1"

line2 :: Text
line2 = "line 2"

parseTmuxTest :: MyoTest ()
parseTmuxTest = do
  lift initialize''
  lift . addEchoHandler =<< fixture "tmux/parse/file"
  ident <- lift (addEchoCommand "tmux" [line1, line2] False)
  lift (myoRunIdent ident)
  await logComplete (fmap (ByteString.lines . CommandLog._current) <$> commandLog ident)
  myoParse $ ParseOptions Nothing Nothing Nothing
  index <- currentLine
  (Just "line 1" ===) =<< Lens.firstOf (Lens.element index) <$> currentBufferContent
  where
    logComplete mayLog = do
      log' <- evalMaybe mayLog
      containsLine ("echoline " <> line2) (decodeUtf8 <$> log')

test_parseTmux :: UnitTest
test_parseTmux =
  tmuxTestDef parseTmuxTest

parseCaptureTmuxTest :: MyoTest ()
parseCaptureTmuxTest = do
  lift initialize''
  lift . addEchoHandler =<< fixture "tmux/parse/file"
  ident <- lift (addEchoCommand "tmux" [line1, line2] True)
  lift (myoRunIdent ident)
  await logComplete (fmap (ByteString.lines . CommandLog._current) <$> logOrCaptureByIdent ident True)
  myoParse $ ParseOptions Nothing Nothing Nothing
  index <- currentLine
  (Just "line 1" ===) =<< Lens.firstOf (Lens.element index) <$> currentBufferContent
  where
    logComplete mayLog = do
      log' <- evalMaybe mayLog
      containsLine ("echoline " <> line2) (decodeUtf8 <$> log')

test_parseCaptureTmux :: UnitTest
test_parseCaptureTmux =
  tmuxTestDef parseCaptureTmuxTest
