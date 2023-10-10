module Myo.Output.Lang.Haskell.Parser where

import Control.Lens (IndexedTraversal')
import Control.Lens.Regex.Text (Match, match, regex)
import Data.Attoparsec.Text (parseOnly)
import Data.Char (digitToInt)
import qualified Data.List.NonEmpty as NonEmpty (last)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Prelude hiding (try)
import Text.Parser.Char (CharParsing, anyChar, char, digit, newline, noneOf, oneOf, text)
import Text.Parser.Combinators (choice, eof, manyTill, sepByNonEmpty, skipMany, skipOptional, try)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)
import Text.Parser.Token (TokenParsing, brackets, natural, parens)

import Myo.Output.Data.Location (Location (Location))
import qualified Myo.Output.Data.OutputError as OutputError (OutputError (Parse))
import Myo.Output.Data.OutputParser (OutputParser (OutputParser))
import Myo.Output.Lang.Haskell.Data.HaskellEvent (EventType, HaskellEvent (HaskellEvent))
import qualified Myo.Output.Lang.Haskell.Data.HaskellEvent as EventType (EventType (..))
import Myo.Output.Lang.Haskell.Report (haskellReport)
import Myo.Regex (regexML, removeControlCharsRE)
import Myo.Text.Parser.Combinators (anyTillChar, colon, emptyLine, minus, skipLine, tillEol, word, ws)

stringAcrossNl ::
  TokenParsing m =>
  String ->
  m Text
stringAcrossNl s =
  toText <$> traverse nlOrChar s
  where
    nlOrChar c =
      skipOptional newline *> char c

number :: TokenParsing m => Integer -> m Char -> m Integer
number base baseDigit =
  foldl' (\ x d -> base * x + toInteger (digitToInt d)) 0 <$> some baseDigit

digitsAcrossNl ::
  TokenParsing m =>
  m Int
digitsAcrossNl =
  fromIntegral <$> number 10 nlOrDigit
  where
    nlOrDigit =
      skipOptional newline *> digit

path ::
  Monad m =>
  CharParsing m =>
  m Text
path =
  toText <$> manyTill anyChar (try (choice [newline, colon]))

pathAcrossNl ::
  Monad m =>
  CharParsing m =>
  m Text
pathAcrossNl = do
  slash <- char '/'
  rest <- toText <$> manyTill (skipOptional newline *> anyChar) (try (choice [newline *> newline, colon]))
  pure (Text.cons slash rest)

normalize :: Integral a => a -> Int
normalize =
  fromIntegral . subtract 1

num ::
  TokenParsing m =>
  m Int
num =
  normalize <$> digitsAcrossNl

lineCol ::
  TokenParsing m =>
  m (Int, Maybe Int)
lineCol =
  (,) <$> line <*> col
  where
    line =
      ws *> num <* ws <* colon
    col =
      Just <$> (ws *> num) <* skipOptional (ws *> minus *> ws *> num)

parensLineCol ::
  TokenParsing m =>
  m (Int, Int)
parensLineCol =
  parens ((,) <$> (num <* ws <* char ',') <*> num)

lineColRegion ::
  TokenParsing m =>
  m (Int, Maybe Int)
lineColRegion =
  second Just <$> parensLineCol <* ws <* minus <* ws <* parensLineCol

location ::
  Monad m =>
  TokenParsing m =>
  m Location
location = do
  p <- pathAcrossNl
  (l, c) <- choice [lineCol, lineColRegion] <* ws
  pure (Location p l c)

locationOneline ::
  Monad m =>
  TokenParsing m =>
  m Location
locationOneline = do
  p <- path
  (l, c) <- choice [lineCol, lineColRegion]
  pure (Location p l c)

locationHeader ::
  Monad m =>
  TokenParsing m =>
  m (Location, EventType)
locationHeader =
  (,) <$> loc <*> tpe
  where
    loc =
      ws *> location <* colon <* ws
    tpe =
      choice [EventType.Error <$ stringAcrossNl "error", EventType.Warning <$ stringAcrossNl "warning"] <* trailing
    trailing =
      ws *> colon *> ws *> skipOptional (brackets (many $ noneOf "]")) *> ws

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
  pure $ toText <$> head' :| tail'
  where
    part =
      ws *> dot *> ws *> manyTill anyChar (choice [void $ lookAhead dot, void emptyLine, codeSnippet, eof])

compileEvent ::
  Monad m =>
  TokenParsing m =>
  LookAheadParsing m =>
  m HaskellEvent
compileEvent =
  uncurry HaskellEvent <$> locationHeader <*> choice [multiMessage, singleMessage]

patternEvent ::
  Monad m =>
  TokenParsing m =>
  m HaskellEvent
patternEvent =
  cons <$> (location <* colon) <*> fun
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
  Monad m =>
  TokenParsing m =>
  m Location
stackFrame =
  ws *>
  word *>
  text "called at" *>
  ws *>
  locationOneline

runtimeEvent ::
  Monad m =>
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

removeProgressIndicator1RE :: IndexedTraversal' Int Text Match
removeProgressIndicator1RE =
  [regex|Progress \d+/\d+: [^ ]+|]

removeProgressIndicator2RE :: IndexedTraversal' Int Text Match
removeProgressIndicator2RE =
  [regex|Progress \d+/\d+|]

removeModulePrefixRE :: IndexedTraversal' Int Text Match
removeModulePrefixRE =
  [regexML|^ *[^> ]+\w *> |]

sanitizeHaskellOutput :: Text -> Text
sanitizeHaskellOutput =
  flip (foldl' @[] \ z r -> z & r . match .~ "") regexes . Text.filter ('\b' /=)
  where
    regexes =
      [removeControlCharsRE, removeProgressIndicator1RE, removeProgressIndicator2RE, removeModulePrefixRE]

haskellOutputParser :: OutputParser r
haskellOutputParser =
  OutputParser \ out -> do
    parsed <- stopEitherWith (OutputError.Parse . toText) (parseOnly parseHaskellErrors (sanitizeHaskellOutput out))
    stopEither (haskellReport parsed)
