module Myo.Output.Lang.Scala.Parser where

import Control.Monad ((<=<))
import Data.Attoparsec.Text (parseOnly)
import Data.Either.Combinators (mapLeft)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Text.Parser.Char (CharParsing, anyChar, char, newline, noneOf, string)
import Text.Parser.Combinators (choice, eof, many, manyTill, notFollowedBy, skipMany, skipOptional, try)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)
import Text.Parser.Token (TokenParsing, brackets, natural, whiteSpace)

import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.OutputError as OutputError (OutputError(Parse))
import Myo.Output.Data.OutputParser (OutputParser(OutputParser))
import Myo.Output.Data.ParsedOutput (ParsedOutput)
import Myo.Output.Lang.Scala.Data.ScalaEvent (EventType, ScalaEvent(ScalaEvent))
import qualified Myo.Output.Lang.Scala.Data.ScalaEvent as EventType (EventType(..))
import Myo.Output.Lang.Scala.Report (scalaReport)
import Myo.Text.Parser.Combinators (colon, emptyLine, skipLine, tillEol, tillInLine, ws)

eventType ::
  TokenParsing m =>
  m EventType
eventType =
  choice [EventType.Error <$ string "error", EventType.Warning <$ string "warn"]

eventTag ::
  TokenParsing m =>
  m EventType
eventTag =
  brackets eventType <* ws

locationLine ::
  Monad m =>
  CharParsing m =>
  TokenParsing m =>
  LookAheadParsing m =>
  m (Location, EventType, Text)
locationLine = do
  tpe <- ws *> eventTag
  path <- tillInLine colon
  lineno <- natural <* colon
  colno <- natural <* colon
  message <- ws *> tillEol
  return (Location path (fromIntegral lineno - 1) (Just (fromIntegral colno - 1)), tpe, toText message)

infoLine ::
  CharParsing m =>
  TokenParsing m =>
  m Text
infoLine =
  eventTag *> (toText <$> tillEol)

colMarkerLine ::
  CharParsing m =>
  TokenParsing m =>
  m ()
colMarkerLine =
  void $ eventTag *> char '^' *> newline

errorInfo ::
  Monad m =>
  CharParsing m =>
  TokenParsing m =>
  LookAheadParsing m =>
  m ([Text], Text)
errorInfo = do
  info <- many (infoLine <* notFollowedBy colMarkerLine)
  code <- infoLine
  void colMarkerLine
  return (info, code)

event ::
  Monad m =>
  CharParsing m =>
  TokenParsing m =>
  LookAheadParsing m =>
  m ScalaEvent
event = do
  (location, eventType, message) <- locationLine
  uncurry (ScalaEvent location eventType message) <$> errorInfo

parseScalaErrors ::
  Monad m =>
  CharParsing m =>
  TokenParsing m =>
  LookAheadParsing m =>
  m (Vector ScalaEvent)
parseScalaErrors =
  Vector.fromList . catMaybes <$> manyTill (choice [Just <$> event, Nothing <$ skipLine]) (ws *> eof)

parseScala :: Text -> Either OutputError ParsedOutput
parseScala =
  scalaReport <=< mapLeft (OutputError.Parse . toText) . parseOnly parseScalaErrors

scalaOutputParser :: OutputParser
scalaOutputParser =
  OutputParser parseScala
