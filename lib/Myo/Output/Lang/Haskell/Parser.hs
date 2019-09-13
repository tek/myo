{-# LANGUAGE QuasiQuotes #-}

module Myo.Output.Lang.Haskell.Parser where

import Data.Attoparsec.Text (parseOnly)
import Data.Either.Combinators (mapLeft)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty (last)
import qualified Data.Text as Text (filter)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Text.Parser.Char (CharParsing, anyChar, char, newline, noneOf, text)
import Text.Parser.Combinators (choice, eof, many, manyTill, sepByNonEmpty, skipMany, skipOptional, try)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)
import Text.Parser.Token (TokenParsing, brackets, natural, parens)
import Text.RE.PCRE.Text (RE, SearchReplace, ed, searchReplaceAll)

import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.OutputError as OutputError (OutputError(Parse))
import Myo.Output.Data.OutputParser (OutputParser(OutputParser))
import Myo.Output.Data.ParsedOutput (ParsedOutput)
import Myo.Output.Lang.Haskell.Data.HaskellEvent (EventType, HaskellEvent(HaskellEvent))
import qualified Myo.Output.Lang.Haskell.Data.HaskellEvent as EventType (EventType(..))
import Myo.Output.Lang.Haskell.Report (haskellReport)
import Myo.Text.Parser.Combinators (anyTillChar, colon, emptyLine, skipLine, tillEol, word, ws)

path ::
  CharParsing m =>
  m Text
path =
  toText <$> manyTill anyChar (try $ choice [newline, colon])

location ::
  TokenParsing m =>
  m Location
location =
  Location <$> path <*> line <*> col
  where
    line =
      num <* colon
    col =
      Just <$> num
    num =
      fromIntegral . subtract 1 <$> natural

locationLine ::
  TokenParsing m =>
  m (Location, EventType)
locationLine =
  tuple loc tpe
  where
    loc =
      ws *> location <* colon <* ws
    tpe =
      choice [EventType.Error <$ text "error", EventType.Warning <$ text "warning"] <* trailing
    trailing =
      colon *> ws *> skipOptional (brackets (many $ noneOf "]")) *> ws

region ::
  TokenParsing m =>
  m Location
region =
  (cons <$> path <*> lineCol) <* (num *> colon)
  where
    cons p (l, c) =
      Location p l c
    lineCol =
      num <* char '-'
    num =
      bimap normalize (Just . normalize) <$> parens (tuple (natural <* char ',') natural)
    normalize =
      fromIntegral . subtract 1

dot :: CharParsing m => m Char
dot =
  char '•'

codeSnippet ::
  TokenParsing m =>
  m ()
codeSnippet =
  void (newline *> ws *> char '|')

singleMessage ::
  Monad m =>
  TokenParsing m =>
  m (NonEmpty Text)
singleMessage =
  pure . toText <$> manyTill anyChar (choice [void emptyLine, codeSnippet, eof])

multiMessage ::
  Monad m =>
  TokenParsing m =>
  LookAheadParsing m =>
  m (NonEmpty Text)
multiMessage = do
  head' <- part
  tail' <- many part
  return $ toText <$> head' :| tail'
  where
    part =
      ws *> dot *> ws *> manyTill anyChar (choice [void $ lookAhead dot, void emptyLine, codeSnippet, eof])

compileEvent ::
  Monad m =>
  TokenParsing m =>
  LookAheadParsing m =>
  m HaskellEvent
compileEvent =
  uncurry HaskellEvent <$> locationLine <*> choice [multiMessage, singleMessage]

patternEvent ::
  TokenParsing m =>
  m HaskellEvent
patternEvent =
  cons <$> region <*> fun
  where
    cons loc =
      HaskellEvent loc EventType.Patterns
    fun =
      ws *> text "Non-exhaustive patterns in function" *> ws *> ((:| []) <$> word)

assertionLine ::
  TokenParsing m =>
  m ()
assertionLine =
  void word *>
  text "failed at" *>
  ws *>
  anyTillChar ':' *>
  natural *>
  ws

stackFrame ::
  TokenParsing m =>
  m Location
stackFrame =
  ws *>
  word *>
  text "called at" *>
  ws *>
  location

runtimeEvent ::
  TokenParsing m =>
  m HaskellEvent
runtimeEvent =
  cons <$> msg <*> loc
  where
    cons m l =
      HaskellEvent l EventType.RuntimeError (m :| [])
    msg =
      skipOptional (assertionLine *> anyTillChar ':') *> ws *> tillEol
    loc =
      NonEmpty.last <$> frames
    frames =
      ws *> text "CallStack" *> void tillEol *> sepByNonEmpty stackFrame tillEol

parseHaskellErrors ::
  Monad m =>
  CharParsing m =>
  TokenParsing m =>
  LookAheadParsing m =>
  m (Vector HaskellEvent)
parseHaskellErrors =
  Vector.fromList . catMaybes <$> many (choice events <* skipMany newline)
  where
    events =
      [
        Just <$> compileEvent,
        Just <$> patternEvent,
        Just <$> runtimeEvent,
        Nothing <$ skipLine
        ]

removeProgressIndicator1RE :: SearchReplace RE Text
removeProgressIndicator1RE =
  [ed|Progress \d+/\d+: [^ ]+///|]

removeProgressIndicator2RE :: SearchReplace RE Text
removeProgressIndicator2RE =
  [ed|Progress \d+/\d+///|]

removeControlCharsRE :: SearchReplace RE Text
removeControlCharsRE =
  [ed|(\x9b|\x1b\[)[0-?]*[ -\/]*[@-~]///|]

sanitizeHaskellOutput :: Text -> Text
sanitizeHaskellOutput =
  flip (foldl (flip searchReplaceAll)) regexes . Text.filter ('\b' /=)
  where
    regexes =
      [removeControlCharsRE, removeProgressIndicator1RE, removeProgressIndicator2RE]

parseHaskell :: Text -> Either OutputError ParsedOutput
parseHaskell =
  haskellReport <=< mapLeft (OutputError.Parse . toText) . parseOnly parseHaskellErrors . sanitizeHaskellOutput

haskellOutputParser :: OutputParser
haskellOutputParser =
  OutputParser parseHaskell
