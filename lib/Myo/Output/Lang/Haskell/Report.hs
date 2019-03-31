module Myo.Output.Lang.Haskell.Report where

import Control.Applicative (Alternative)
import Control.Lens (ifolded, toListOf, views, withIndex, (^..))
import Control.Monad (join)
import Data.Attoparsec.Text (Parser, parseOnly)
import qualified Data.ByteString.Char8 as ByteString (pack)
import Data.Either.Combinators (mapLeft)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Text as Text (pack)
import Text.Parser.Char (CharParsing, anyChar, char, newline, noneOf, string)
import Text.Parser.Combinators (between, choice, many, manyTill, notFollowedBy, sepBy1, skipMany, skipOptional, some, try)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)
import Text.Parser.Token (TokenParsing, brackets, natural, token, whiteSpace)

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
  FoundReq1 String String
  |
  FoundReq2 String String
  |
  TypeNotInScope String
  |
  Verbatim String
  |
  NoMethod String
  deriving (Eq, Show)

qname :: TokenParsing m => m String
qname =
  unwords <$> between (char '‘') (char '’') (sepBy1 (some (noneOf "\n ’")) whiteSpace)

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
  Verbatim <$> many anyChar

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

formatMessage :: HaskellMessage -> [String]
formatMessage (FoundReq1 found req) =
  ["type mismatch", found, req]
formatMessage (FoundReq2 found req) =
  ["type mismatch", found, req]
formatMessage (TypeNotInScope tpe) =
  ["type not in scope: " ++ tpe]
formatMessage (NoMethod meth) =
  ["method not implemented: " ++ meth]
formatMessage (Verbatim text) =
  [text]

formatLocation :: Location -> String
formatLocation (Location path line _) =
  unwords [path, lineNumber, show (line + 1)]

formatReportLine :: Int -> Location -> HaskellMessage -> [ReportLine]
formatReportLine index location message =
  ReportLine index . Text.pack <$> (formatLocation location : formatMessage message ++ [""])

formatEvent :: Location -> Int -> Int -> HaskellMessage -> (OutputEvent, [ReportLine])
formatEvent location level index message =
  (OutputEvent (Just location) level, formatReportLine index location message)

parsedOutputCons :: [HaskellOutputEvent] -> Int -> ParseReport
parsedOutputCons events offset =
  ParseReport events' (join lines')
  where
    (events', lines') = unzip (cons <$> events)
    cons HaskellOutputEvent{..} =
      formatEvent location level (index + offset) message

eventLevel :: EventType -> Int
eventLevel EventType.Warning = 1
eventLevel EventType.Error = 0

eventReport :: Int -> HaskellEvent -> Either OutputError HaskellOutputEvent
eventReport index (HaskellEvent loc tpe (messageText :| _)) = do
  message <- mapLeft OutputError.Parse $ parseOnly parseMessage messageText
  return $ HaskellOutputEvent (eventLevel tpe) index loc message

haskellReport :: [HaskellEvent] -> Either OutputError ParsedOutput
haskellReport events =
  ParsedOutput haskellSyntax . parsedOutputCons <$> (traverse (uncurry eventReport) . zipWithIndex) events
  where
    zipWithIndex = toListOf (ifolded . withIndex)
