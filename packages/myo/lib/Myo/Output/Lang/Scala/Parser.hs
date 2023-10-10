module Myo.Output.Lang.Scala.Parser where

import Data.Attoparsec.Text (parseOnly)
import Data.Char (isSpace)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Text.Parser.Char (char, newline, satisfy, string)
import Text.Parser.Combinators (choice, notFollowedBy, skipMany, skipOptional)
import Text.Parser.Token (TokenParsing, natural)

import Myo.Output.Data.Location (Location (Location))
import qualified Myo.Output.Data.OutputError as OutputError (OutputError (Parse))
import Myo.Output.Data.OutputParser (OutputParser (OutputParser))
import Myo.Output.Lang.Scala.Data.ScalaEvent (EventType, ScalaEvent (ScalaEvent))
import qualified Myo.Output.Lang.Scala.Data.ScalaEvent as EventType (EventType (..))
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
  TokenParsing m =>
  m (Location, EventType, Text)
locationLine = do
  tpe <- ws *> eventStart <* char ' ' <* notFollowedBy (char ' ')
  path <- tillInLine colon
  lineno <- natural <* colon
  colno <- natural <* colon
  message <- ws *> tillEol
  pure (Location path (fromIntegral lineno - 1) (Just (fromIntegral colno - 1)), tpe, toText message)

infoLine ::
  TokenParsing m =>
  m Text
infoLine =
  eventStartSkipWs *> (toText <$> tillEol)

codeLine ::
  Monad m =>
  TokenParsing m =>
  m (Int, Text)
codeLine = do
  skipUntagged
  w <- char '[' *> eventType *> char ']' *> many (satisfy isSpace)
  t <- toText <$> tillEol
  pure (length w - 1, t)

colMarkerLine ::
  TokenParsing m =>
  m ()
colMarkerLine =
  void $ eventStartSkipWs *> char '^' *> newline

errorInfo ::
  Monad m =>
  TokenParsing m =>
  m ([Text], Int, Text)
errorInfo = do
  info <- many (infoLine <* skipUntagged <* notFollowedBy (choice [colMarkerLine, void locationLine]))
  (indent, code) <- codeLine
  skipOptional colMarkerLine
  pure (info, indent, code)

event ::
  Monad m =>
  TokenParsing m =>
  m ScalaEvent
event = do
  (location, eventType', message) <- locationLine
  (info, indent, code) <- errorInfo
  pure (ScalaEvent location eventType' message info indent code)

parseScalaErrors ::
  Monad m =>
  TokenParsing m =>
  m (Vector ScalaEvent)
parseScalaErrors =
  Vector.fromList . catMaybes <$> many (choice [Just <$> event, Nothing <$ skipLine])

scalaOutputParser :: OutputParser r
scalaOutputParser =
  OutputParser \ out -> do
    events <- stopEitherWith (OutputError.Parse . toText) (parseOnly parseScalaErrors out)
    stopEither (scalaReport events)
