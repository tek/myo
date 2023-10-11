module Myo.Test.Output.Echo where

import Control.Lens (IndexedTraversal')
import Control.Lens.Regex.Text (Match, group, regex)
import qualified Data.Text as Text (stripPrefix)
import qualified Data.Vector as Vector (fromList, singleton)
import Path (Abs, File, Path)
import Prelude hiding (group)
import Ribosome (Handler)
import Ribosome.Host.Path (pathText)

import Myo.Command.Add (myoAddSystemCommand)
import qualified Myo.Command.Data.AddSystemCommandOptions as AddSystemCommandOptions
import Myo.Command.Data.AddSystemCommandOptions (capture, lang, target)
import Myo.Command.Data.Command (CommandLanguage (CommandLanguage))
import Myo.Command.Data.CommandError (CommandError)
import Myo.Data.CommandId (CommandId)
import Myo.Effect.Commands (Commands)
import qualified Myo.Output.Data.EventIndex as EventIndex (Relative (Relative))
import Myo.Output.Data.Location (Location (Location))
import Myo.Output.Data.OutputEvent (OutputEvent (OutputEvent), OutputEventMeta (OutputEventMeta))
import Myo.Output.Data.OutputEvents (OutputEvents (OutputEvents))
import Myo.Output.Data.ParsedOutput (ParsedOutput (ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine (ReportLine))

stripPromptRegex :: IndexedTraversal' Int Text Match
stripPromptRegex =
  [regex|(.*\$ ).*|]

stripPrompt :: Text -> Text
stripPrompt =
  stripPromptRegex . group 0 .~ ""

echoLang :: CommandLanguage
echoLang =
  CommandLanguage "echo"

parseEcho ::
  Path Abs File ->
  Text ->
  Sem r ParsedOutput
parseEcho file text' =
  pure (ParsedOutput def events)
  where
    events =
      OutputEvents (Vector.fromList (uncurry event <$> zip matching [0..]))
    matching =
      catMaybes $ Text.stripPrefix "echoline " . stripPrompt <$> lines text'
    event lineText index =
      OutputEvent eventMeta (Vector.singleton (rline lineText index))
    rline lineText index =
      ReportLine (EventIndex.Relative index) lineText
    eventMeta =
      OutputEventMeta (Just (Location (pathText file) 0 Nothing)) 0

addEchoCommand ::
  Member (Commands !! CommandError) r =>
  [Text] ->
  Bool ->
  Handler r CommandId
addEchoCommand lines' capture =
  ident <$ myoAddSystemCommand opts
  where
    opts =
      (AddSystemCommandOptions.cons ident cmds) {
        target = Just "make",
        lang = Just echoLang,
        capture = Just capture
      }
    ident =
      "cmd"
    cmds =
      cmd <$> lines'
    cmd line' =
      "echo echoline " <> line'
