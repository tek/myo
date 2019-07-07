module Myo.Output.Lang.Report where

import Control.Lens (ifolded, view, withIndex)
import Data.Attoparsec.Text (parseOnly)
import qualified Data.List as List (unwords)
import qualified Data.Text as Text (splitAt)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList, unzip)
import Data.Vector.Lens (toVectorOf)
import Text.Parser.Char (CharParsing, char, noneOf)
import Text.Parser.Combinators (between, choice, many, sepBy1, skipOptional, some, try)
import Text.Parser.Token (TokenParsing, brackets, parens, whiteSpace)

import qualified Myo.Output.Data.EventIndex as EventIndex (Absolute(Absolute), Relative(Relative))
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputError (OutputError)
import Myo.Output.Data.OutputEvent (LangOutputEvent(LangOutputEvent), OutputEvent(OutputEvent))
import qualified Myo.Output.Data.OutputEvent as LangOutputEvent (event)
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine(ReportLine))
import Myo.Output.Data.String (colMarker, lineNumber)
import Myo.Output.Lang.Scala.Data.ScalaEvent (EventType, ScalaEvent(ScalaEvent))
import qualified Myo.Output.Lang.Scala.Data.ScalaEvent as EventType (EventType(..))
import Myo.Output.Lang.Scala.Syntax (foundMarker, reqMarker, scalaSyntax, separatorMarker)

parsedOutputCons ::
  (LangOutputEvent a -> Vector Text) ->
  Vector (LangOutputEvent a) ->
  ParseReport EventIndex.Relative
parsedOutputCons format events =
  ParseReport events' lines'
  where
    events' =
      view LangOutputEvent.event <$> events
    lines' :: Vector (ReportLine EventIndex.Relative)
    lines' =
      uncurry line' =<< zipWithIndex events
    line' index event =
      ReportLine (EventIndex.Relative (fromIntegral index)) <$> format event
    zipWithIndex =
      toVectorOf (ifolded . withIndex)
