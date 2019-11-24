module Myo.Output.Lang.Scala.Parser where

import Control.Monad ((<=<))
import Data.Attoparsec.Text (parseOnly)
import Data.Char (isSpace)
import Data.Either.Combinators (mapLeft)
import Data.Functor (void)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Text.Parser.Char (CharParsing, char, newline, satisfy, string)
import Text.Parser.Combinators (choice, many, notFollowedBy, skipMany, skipOptional)
import Text.Parser.LookAhead (LookAheadParsing)
import Text.Parser.Token (TokenParsing, brackets, natural)

import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.OutputError as OutputError (OutputError(Parse))
import Myo.Output.Data.OutputParser (OutputParser(OutputParser))
import Myo.Output.Data.ParsedOutput (ParsedOutput)
import Myo.Output.Lang.Scala.Data.ScalaEvent (EventType, ScalaEvent(ScalaEvent))
import qualified Myo.Output.Lang.Scala.Data.ScalaEvent as EventType (EventType(..))
import Myo.Output.Lang.Scala.Report (scalaReport)
import Myo.Text.Parser.Combinators (colon, skipLine, tillEol, tillInLine, ws)

eventType ::
  TokenParsing m =>
  m EventType
eventType =
  choice [EventType.Error <$ string "error", EventType.Warning <$ string "warn"]

eventTag ::
  TokenParsing m =>
  m EventType
eventTag =
  ws *> char '[' *> eventType <* char ']'

skipUntagged ::
  TokenParsing m =>
  m ()
skipUntagged =
  skipMany (notFollowedBy eventTag *> tillEol)

eventStart ::
  TokenParsing m =>
  m EventType
eventStart =
  skipUntagged *> eventTag

eventStartSkipWs ::
  TokenParsing m =>
  m EventType
eventStartSkipWs =
  skipUntagged *> eventTag <* ws

locationLine ::
  Monad m =>
  CharParsing m =>
  TokenParsing m =>
  LookAheadParsing m =>
  m (Location, EventType, Text)
locationLine = do
  tpe <- ws *> eventStart <* char ' ' <* notFollowedBy (char ' ')
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
  eventStartSkipWs *> (toText <$> tillEol)

codeLine ::
  Monad m =>
  CharParsing m =>
  TokenParsing m =>
  m (Int, Text)
codeLine = do
  skipUntagged
  w <- char '[' *> eventType *> char ']' *> many (satisfy isSpace)
  t <- toText <$> tillEol
  return (length w - 1, t)

colMarkerLine ::
  CharParsing m =>
  TokenParsing m =>
  m ()
colMarkerLine =
  void $ eventStartSkipWs *> char '^' *> newline

errorInfo ::
  Monad m =>
  CharParsing m =>
  TokenParsing m =>
  LookAheadParsing m =>
  m ([Text], Int, Text)
errorInfo = do
  info <- many (infoLine <* skipUntagged <* notFollowedBy (choice [colMarkerLine, void locationLine]))
  (indent, code) <- codeLine
  skipOptional colMarkerLine
  return (info, indent, code)

event ::
  Monad m =>
  CharParsing m =>
  TokenParsing m =>
  LookAheadParsing m =>
  m ScalaEvent
event = do
  (location, eventType', message) <- locationLine
  (info, indent, code) <- errorInfo
  return (ScalaEvent location eventType' message info indent code)

parseScalaErrors ::
  Monad m =>
  CharParsing m =>
  TokenParsing m =>
  LookAheadParsing m =>
  m (Vector ScalaEvent)
parseScalaErrors =
  Vector.fromList . catMaybes <$> many (choice [Just <$> event, Nothing <$ skipLine])

parseScala :: Text -> Either OutputError ParsedOutput
parseScala =
  scalaReport <=< mapLeft (OutputError.Parse . toText) . parseOnly parseScalaErrors

scalaOutputParser :: OutputParser
scalaOutputParser =
  OutputParser parseScala
