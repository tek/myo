module Output.Cat where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import qualified Data.Vector as Vector (fromList, singleton)

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.Command (CommandLanguage(CommandLanguage))
import Myo.Command.Parse (addHandler)
import Myo.Data.Env (Myo)
import qualified Myo.Output.Data.EventIndex as EventIndex (Relative(Relative))
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputError (OutputError)
import Myo.Output.Data.OutputEvent (OutputEvent(OutputEvent), OutputEventMeta(OutputEventMeta))
import Myo.Output.Data.OutputEvents (OutputEvents(OutputEvents))
import Myo.Output.Data.OutputHandler (OutputHandler(OutputHandler))
import Myo.Output.Data.OutputParser (OutputParser(OutputParser))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine(ReportLine))

lang :: CommandLanguage
lang = CommandLanguage "echo"

parseCat :: Text -> Text -> Either OutputError ParsedOutput
parseCat file text =
  Right (ParsedOutput def events)
  where
    events =
      OutputEvents (Vector.fromList (event <$> (rline <$> lines text)))
    event l =
      OutputEvent eventMeta (Vector.singleton l)
    rline =
      ReportLine (EventIndex.Relative 0)
    eventMeta =
      OutputEventMeta (Just (Location file 0 Nothing)) 0

addCatHandler :: FilePath -> Myo ()
addCatHandler file =
  addHandler lang (OutputHandler (OutputParser (parseCat (toText file))))

addCatCommand :: Text -> Myo Ident
addCatCommand runner =
  ident <$ myoAddSystemCommand opts
  where
    opts =
      AddSystemCommandOptions ident cmds (Just (Ident.Str runner)) (Just (Ident.Str "make")) (Just lang) Nothing Nothing
    ident =
      Ident.Str "cat"
    cmds =
      ["cat"]
