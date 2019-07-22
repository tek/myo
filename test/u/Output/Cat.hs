module Output.Cat where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
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

parseCat :: Text -> Text -> Either OutputError ParsedOutput
parseCat file text =
  Right (ParsedOutput def report)
  where
    report =
      ParseReport (Vector.fromList events) (Vector.fromList (rline <$> lines text))
    rline =
      ReportLine (EventIndex.Relative 0)
    events =
      event <$ lines text
    event =
      OutputEvent (Just (Location file 0 Nothing)) 0

addCatHandler :: FilePath -> Myo ()
addCatHandler file =
  addHandler lang (OutputHandler (OutputParser (parseCat (toText file))))

addCatCommand :: Text -> Myo Ident
addCatCommand runner =
  ident <$ myoAddSystemCommand opts
  where
    opts =
      AddSystemCommandOptions ident cmds (Just (Ident.Str runner)) (Just (Ident.Str "make")) (Just lang) Nothing
    ident =
      Ident.Str "cat"
    cmds =
      ["cat"]
