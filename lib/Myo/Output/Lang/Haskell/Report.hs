module Myo.Output.Lang.Haskell.Report where

import Control.Lens (ifolded, withIndex)
import Control.Monad (join)
import Data.Attoparsec.Text (parseOnly)
import Data.Either.Combinators (mapLeft)
import qualified Data.List as List (unwords)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList, unzip)
import Data.Vector.Lens (toVectorOf)
import Text.Parser.Char (CharParsing, anyChar, char, noneOf, string)
import Text.Parser.Combinators (between, choice, many, sepBy1, skipMany, skipOptional, some)
import Text.Parser.Token (TokenParsing, whiteSpace)

import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.OutputError as OutputError (OutputError(Parse))
import Myo.Output.Data.OutputEvent (OutputEvent(OutputEvent))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine(ReportLine))
import Myo.Output.Data.String (lineNumber)
import Myo.Output.Lang.Haskell.Data.HaskellEvent (EventType, HaskellEvent(HaskellEvent))
import qualified Myo.Output.Lang.Haskell.Data.HaskellEvent as EventType (EventType(..))
import Myo.Output.Lang.Haskell.Syntax (haskellSyntax)

data HaskellMessage =
  FoundReq1 Text Text
  |
  FoundReq2 Text Text
  |
  TypeNotInScope Text
  |
  Verbatim Text
  |
  NoMethod Text
  deriving (Eq, Show)

qname :: TokenParsing m => m Text
qname =
  toText . List.unwords <$> between (char '‘') (char '’') (sepBy1 (some (noneOf "\n ’")) whiteSpace)

ws :: TokenParsing m => m ()
ws =
  skipOptional whiteSpace

foundReq1 ::
  Applicative m =>
  TokenParsing m =>
  m HaskellMessage
foundReq1 =
  flip FoundReq1 <$> req <*> found
  where
    req = string "Couldn't match expected type" *> ws *> qname <* ws
    found = string "with actual type" *> ws *> qname <* skipMany anyChar

foundReq2 ::
  Monad m =>
  TokenParsing m =>
  m HaskellMessage
foundReq2 = do
  found <- string "Couldn't match type " *> qname
  req <- string " with " *> skipOptional (string "actual type ") *> qname
  skipMany anyChar
  return $ FoundReq2 found req

typeNotInScope ::
  TokenParsing m =>
  m HaskellMessage
typeNotInScope =
  string "Not in scope: type constructor or class" *> ws *> (TypeNotInScope <$> qname)

noMethod ::
  TokenParsing m =>
  m HaskellMessage
noMethod =
  string "No explicit implementation for" *> ws *> (NoMethod <$> qname) <* skipMany anyChar

verbatim :: CharParsing m => m HaskellMessage
verbatim =
  Verbatim . toText <$> many anyChar

parseMessage ::
  TokenParsing m =>
  Monad m =>
  m HaskellMessage
parseMessage =
  choice [foundReq1, foundReq2, typeNotInScope, noMethod, verbatim]

data HaskellOutputEvent =
  HaskellOutputEvent {
    level :: Int,
    index :: Int,
    location :: Location,
    message :: HaskellMessage
    }
  deriving (Eq, Show)

formatMessage :: HaskellMessage -> [Text]
formatMessage (FoundReq1 found req) =
  ["type mismatch", found, req]
formatMessage (FoundReq2 found req) =
  ["type mismatch", found, req]
formatMessage (TypeNotInScope tpe) =
  ["type not in scope: " <> tpe]
formatMessage (NoMethod meth) =
  ["method not implemented: " <> meth]
formatMessage (Verbatim text) =
  [text]

formatLocation :: Location -> Text
formatLocation (Location path line _) =
  unwords [toText path, lineNumber, show (line + 1)]

formatReportLine :: Int -> Location -> HaskellMessage -> Vector ReportLine
formatReportLine index location message =
  ReportLine index <$> Vector.fromList (formatLocation location : formatMessage message <> [""])

formatEvent :: Location -> Int -> Int -> HaskellMessage -> (OutputEvent, Vector ReportLine)
formatEvent location level index message =
  (OutputEvent (Just location) level, formatReportLine index location message)

parsedOutputCons :: Vector HaskellOutputEvent -> Int -> ParseReport
parsedOutputCons events offset =
  ParseReport events' (join lines')
  where
    (events', lines') = Vector.unzip (cons <$> events)
    cons HaskellOutputEvent{..} =
      formatEvent location level (index + offset) message

eventLevel :: EventType -> Int
eventLevel EventType.Warning = 1
eventLevel EventType.Error = 0

eventReport :: Int -> HaskellEvent -> Either OutputError HaskellOutputEvent
eventReport index (HaskellEvent loc tpe (messageText :| _)) = do
  message <- mapLeft (OutputError.Parse . toText) $ parseOnly parseMessage messageText
  return $ HaskellOutputEvent (eventLevel tpe) index loc message

haskellReport :: Vector HaskellEvent -> Either OutputError ParsedOutput
haskellReport events =
  ParsedOutput haskellSyntax . parsedOutputCons <$> (traverse (uncurry eventReport) . zipWithIndex) events
  where
    zipWithIndex = toVectorOf (ifolded . withIndex)
