{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tmux.ParseSpec (htf_thisModulesTests) where

import Chiasma.Data.Ident (Ident(Str))
import qualified Control.Lens as Lens (element, firstOf)
import qualified Data.ByteString.Char8 as ByteString (lines)
import Data.Default (def)
import Data.Text (Text)
import qualified Data.Vector as Vector (fromList)
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Api.Window (currentLine)
import Ribosome.Test.Await (await)
import Test.Framework

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.Command (CommandLanguage(CommandLanguage))
import qualified Myo.Command.Data.CommandLog as CommandLog (_current)
import Myo.Command.Data.ParseOptions (ParseOptions(ParseOptions))
import Myo.Command.Log (commandLog)
import Myo.Command.Parse (addHandler, myoParse)
import Myo.Command.Run (myoRun)
import Myo.Data.Env (Myo)
import Myo.Init (initialize'')
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputError (OutputError)
import Myo.Output.Data.OutputEvent (EventIndex(EventIndex), OutputEvent(OutputEvent))
import Myo.Output.Data.OutputHandler (OutputHandler(OutputHandler))
import Myo.Output.Data.OutputParser (OutputParser(OutputParser))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine(ReportLine))
import Unit (tmuxSpecDef)

line1 :: Text
line1 = "line 1"

line2 :: Text
line2 = "line 2"

lang :: CommandLanguage
lang = CommandLanguage "echo"

parseEcho :: Text -> Text -> Either OutputError ParsedOutput
parseEcho file text =
  Right (ParsedOutput def (const report))
  where
    report =
      ParseReport (Vector.fromList events) (Vector.fromList (rline <$> lines text))
    rline =
      ReportLine (EventIndex 0)
    events =
      [OutputEvent (Just (Location file 0 Nothing)) 0]

parseTmuxSpec :: Myo ()
parseTmuxSpec = do
  let
    ident = Str "cmd"
    cmds = ["echo '" <> line1 <> "'", "echo '" <> line2 <> "'"]
    opts = AddSystemCommandOptions ident cmds (Just (Str "tmux")) (Just (Str "make")) (Just lang) Nothing
  file <- fixture "tmux/parse/file"
  initialize''
  addHandler lang (OutputHandler (OutputParser (parseEcho (toText file))))
  myoAddSystemCommand opts
  myoRun ident
  sleep 2
  log' <- await gassertJust (ByteString.lines . CommandLog._current <$$> commandLog ident)
  gassertBool $ encodeUtf8 line2 `elem` log'
  myoParse $ ParseOptions Nothing Nothing Nothing
  index <- currentLine
  gassertEqual (Just "line 1") =<< Lens.firstOf (Lens.element index) <$> currentBufferContent

test_parseTmux :: IO ()
test_parseTmux =
  tmuxSpecDef parseTmuxSpec
