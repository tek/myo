module Myo.Output.Lang.Haskell.Parser where

import Data.Attoparsec.Text (parseOnly)
import qualified Data.List.NonEmpty as NonEmpty (last)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Text.Parser.Char (CharParsing, anyChar, char, newline, noneOf, oneOf, text)
import Text.Parser.Combinators (choice, eof, manyTill, sepByNonEmpty, skipMany, skipOptional, try)
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
import Myo.Text.Parser.Combinators (anyTillChar, colon, emptyLine, minus, skipLine, tillEol, word, ws)

acrossNl ::
  TokenParsing m =>
  String ->
  m Text
acrossNl s =
  toText <$> traverse nlOrChar s
  where
    nlOrChar c =
      skipOptional newline *> char c

path ::
  CharParsing m =>
  m Text
path = do
  slash <- char '/'
  rest <- toText <$> manyTill (skipOptional newline *> anyChar) (try (choice [newline *> newline, colon]))
  pure (Text.cons slash rest)

location ::
  TokenParsing m =>
  m Location
location =
  Location <$> path <*> line <*> col
  where
    line =
      ws *> num <* ws <* colon
    col =
      Just <$> (ws *> num) <* (skipOptional (ws *> minus *> ws *> num))
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
      choice [EventType.Error <$ acrossNl "error", EventType.Warning <$ acrossNl "warning"] <* trailing
    trailing =
      ws *> colon *> ws *> skipOptional (brackets (many $ noneOf "]")) *> ws

region ::
  TokenParsing m =>
  m Location
region =
  (cons <$> path <*> lineCol) <* (num *> colon)
  where
    cons p (l, c) =
      Location p l c
    lineCol =
      num <* minus
    num =
      bimap normalize (Just . normalize) <$> parens (tuple (natural <* char ',') natural)
    normalize =
      fromIntegral . subtract 1

dot :: CharParsing m => m Char
dot =
  oneOf "•*"

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

removeModulePrefixRE :: SearchReplace RE Text
removeModulePrefixRE =
  [ed|^ *[^> ]+\w *> ///|]

sanitizeHaskellOutput :: Text -> Text
sanitizeHaskellOutput =
  flip (foldl (flip searchReplaceAll)) regexes . Text.filter ('\b' /=)
  where
    regexes =
      [removeControlCharsRE, removeProgressIndicator1RE, removeProgressIndicator2RE, removeModulePrefixRE]

parseHaskell :: Text -> Either OutputError ParsedOutput
parseHaskell =
  haskellReport <=< mapLeft (OutputError.Parse . toText) . parseOnly parseHaskellErrors . sanitizeHaskellOutput

haskellOutputParser :: OutputParser
haskellOutputParser =
  OutputParser parseHaskell
