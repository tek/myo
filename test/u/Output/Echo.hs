module Output.Echo where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import qualified Data.Text as Text (stripPrefix)
import qualified Data.Vector as Vector (fromList)

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.Command (CommandLanguage(CommandLanguage))
import Myo.Command.Parse (addHandler)
import Myo.Data.Env (Myo)
import qualified Myo.Output.Data.EventIndex as EventIndex (Relative(Relative))
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputError (OutputError)
import Myo.Output.Data.OutputEvent (OutputEvent(OutputEvent))
import Myo.Output.Data.OutputHandler (OutputHandler(OutputHandler))
import Myo.Output.Data.OutputParser (OutputParser(OutputParser))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine(ReportLine))

lang :: CommandLanguage
lang = CommandLanguage "echo"

parseEcho :: Text -> Text -> Either OutputError ParsedOutput
parseEcho file text =
  Right (ParsedOutput def report)
  where
    report =
      ParseReport (Vector.fromList events) (Vector.fromList (uncurry rline <$> zip matching [0..]))
    rline lineText index =
      ReportLine (EventIndex.Relative index) lineText
    matching =
      catMaybes $ Text.stripPrefix "echoline " <$> lines text
    events =
      event <$ matching
    event =
      OutputEvent (Just (Location file 0 Nothing)) 0

addEchoHandler :: FilePath -> Myo ()
addEchoHandler file =
  addHandler lang (OutputHandler (OutputParser (parseEcho (toText file))))

addEchoCommand :: Text -> [Text] -> Myo Ident
addEchoCommand runner lines' =
  ident <$ myoAddSystemCommand opts
  where
    opts =
      AddSystemCommandOptions ident cmds (Just (Ident.Str runner)) (Just (Ident.Str "make")) (Just lang) Nothing
    ident =
      Ident.Str "cmd"
    cmds =
      cmd <$> lines'
    cmd line' =
      "echo echoline " <> line'
