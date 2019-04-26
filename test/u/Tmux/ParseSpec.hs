{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tmux.ParseSpec (htf_thisModulesTests) where

import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Test.Tmux (sleep)
import qualified Data.ByteString.Char8 as ByteString (lines)
import Data.Default (def)
import Data.Text (Text)
import qualified Data.Vector as Vector (fromList)
import Test.Framework

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.Command (CommandLanguage(CommandLanguage))
import qualified Myo.Command.Data.CommandLog as CommandLog (_current)
import Myo.Command.Data.ParseOptions (ParseOptions(ParseOptions))
import Myo.Command.Log (commandLog)
import Myo.Command.Parse (addHandler, myoParse)
import Myo.Command.Run (myoRun)
import Myo.Data.Env (MyoN)
import Myo.Init (initialize'')
import Myo.Output.Data.OutputError (OutputError)
import Myo.Output.Data.OutputEvent (EventIndex(EventIndex), OutputEvent(OutputEvent))
import Myo.Output.Data.OutputHandler (OutputHandler(OutputHandler))
import Myo.Output.Data.OutputParser (OutputParser(OutputParser))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine(ReportLine))
import Myo.Tmux.Runner (addTmuxRunner)
import Unit (tmuxGuiSpecDef)

line1 :: Text
line1 = "line 1"

line2 :: Text
line2 = "line 2"

lang :: CommandLanguage
lang = CommandLanguage "echo"

parseEcho :: Text -> Either OutputError ParsedOutput
parseEcho text =
  Right (ParsedOutput def (const report))
  where
    report =
      ParseReport (Vector.fromList [OutputEvent Nothing 0]) (Vector.fromList (rline <$> lines text))
    rline = ReportLine (EventIndex 0)

parseTmuxSpec :: MyoN ()
parseTmuxSpec = do
  let
    ident = Str "cmd"
    cmds = ["echo '" <> line1 <> "'", "echo '" <> line2 <> "'"]
    opts = AddSystemCommandOptions ident cmds (Just (Str "tmux")) (Just (Str "make")) (Just lang)
  initialize''
  addHandler lang (OutputHandler (OutputParser parseEcho))
  addTmuxRunner
  myoAddSystemCommand opts
  myoRun ident
  sleep 2
  mayLog <- commandLog ident
  log' <- ByteString.lines . CommandLog._current <$> gassertJust mayLog
  dbgs log'
  gassertBool $ encodeUtf8 line2 `elem` log'
  myoParse $ ParseOptions Nothing Nothing Nothing

test_parseTmux :: IO ()
test_parseTmux =
  tmuxGuiSpecDef parseTmuxSpec
