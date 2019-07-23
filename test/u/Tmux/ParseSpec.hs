{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tmux.ParseSpec (htf_thisModulesTests) where

import qualified Control.Lens as Lens (element, firstOf)
import qualified Data.ByteString.Char8 as ByteString (lines)
import Prelude hiding (tmuxSpecDef)
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Api.Window (currentLine)
import Ribosome.Test.Await (await)
import Test.Framework

import qualified Myo.Command.Data.CommandLog as CommandLog (_current)
import Myo.Command.Data.ParseOptions (ParseOptions(ParseOptions))
import Myo.Command.Log (commandLog)
import Myo.Command.Parse (myoParse)
import Myo.Command.Run (myoRun)
import Myo.Data.Env (Myo)
import Myo.Init (initialize'')
import Output.Echo (addEchoCommand, addEchoHandler)
import Unit (tmuxSpecDef)

line1 :: Text
line1 = "line 1"

line2 :: Text
line2 = "line 2"

parseTmuxSpec :: Myo ()
parseTmuxSpec = do
  initialize''
  addEchoHandler =<< fixture "tmux/parse/file"
  ident <- addEchoCommand "tmux" [line1, line2]
  myoRun ident
  await logComplete (ByteString.lines . CommandLog._current <$$> commandLog ident)
  myoParse $ ParseOptions Nothing Nothing Nothing
  index <- currentLine
  gassertEqual (Just "line 1") =<< Lens.firstOf (Lens.element index) <$> currentBufferContent
  where
    logComplete mayLog = do
      log' <- gassertJust mayLog
      gassertBool $ encodeUtf8 ("echoline " <> line2) `elem` log'

test_parseTmux :: IO ()
test_parseTmux =
  tmuxSpecDef parseTmuxSpec
