module Myo.Output.Lang.Haskell.Parser where

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
import Text.Parser.Combinators (choice, eof, many, manyTill, skipMany, skipOptional, try)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)
import Text.Parser.Token (TokenParsing, brackets, natural, whiteSpace)

import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.OutputError as OutputError (OutputError(Parse))
import Myo.Output.Data.OutputParser (OutputParser(OutputParser))
import Myo.Output.Data.ParsedOutput (ParsedOutput)
import Myo.Output.Lang.Haskell.Data.HaskellEvent (EventType, HaskellEvent(HaskellEvent))
import qualified Myo.Output.Lang.Haskell.Data.HaskellEvent as EventType (EventType(..))
import Myo.Output.Lang.Haskell.Report (haskellReport)
import Myo.Text.Parser.Combinators (colon, emptyLine, skipLine, ws)

locationLine ::
  Monad m =>
  CharParsing m =>
  TokenParsing m =>
  m (Location, EventType)
locationLine = do
  path <- manyTill anyChar (try $ choice [newline, colon])
  lineno <- natural <* colon
  colno <- natural <* colon
  skipOptional whiteSpace
  tpe <- choice [EventType.Error <$ string "error", EventType.Warning <$ string "warning"]
  _ <- colon
  ws
  skipOptional (brackets (many $ noneOf "]"))
  ws
  return (Location path (fromIntegral lineno - 1) (Just (fromIntegral colno - 1)), tpe)

dot :: CharParsing m => m Char
dot =
  char '•'

singleMessage ::
  Monad m =>
  CharParsing m =>
  m (NonEmpty Text)
singleMessage =
  pure . toText <$> manyTill anyChar (choice [void emptyLine, eof])

multiMessage ::
  Monad m =>
  CharParsing m =>
  TokenParsing m =>
  LookAheadParsing m =>
  m (NonEmpty Text)
multiMessage = do
  head' <- part
  tail' <- many part
  return $ toText <$> head' :| tail'
  where
    part = ws *> dot *> ws *> manyTill anyChar (choice [void $ lookAhead dot, void emptyLine, eof])

event ::
  Monad m =>
  CharParsing m =>
  TokenParsing m =>
  LookAheadParsing m =>
  m HaskellEvent
event = do
  (location, tpe) <- locationLine
  msgs <- choice [multiMessage, singleMessage]
  skipMany newline
  return $ HaskellEvent location tpe msgs

parseHaskellErrors ::
  Monad m =>
  CharParsing m =>
  TokenParsing m =>
  LookAheadParsing m =>
  m (Vector HaskellEvent)
parseHaskellErrors =
  Vector.fromList . catMaybes <$> many (choice [Just <$> event, Nothing <$ skipLine])

parseHaskell :: Text -> Either OutputError ParsedOutput
parseHaskell =
  haskellReport <=< mapLeft (OutputError.Parse . toText) . parseOnly parseHaskellErrors

haskellOutputParser :: OutputParser
haskellOutputParser =
  OutputParser parseHaskell
