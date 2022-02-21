module Myo.Output.Lang.Nix.Parser where

import Data.Attoparsec.Text (parseOnly)
import Data.Char (digitToInt)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Prelude hiding (text)
import Text.Parser.Char (CharParsing, anyChar, char, newline, text)
import Text.Parser.Combinators (choice, manyTill, skipMany, skipOptional, try)
import Text.Parser.Token (TokenParsing, integer, natural)
import Text.RE.PCRE.Text (RE, SearchReplace, ed, searchReplaceAll)

import Myo.Output.Data.Location (Location (Location))
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.OutputError as OutputError (OutputError (Parse))
import Myo.Output.Data.OutputParser (OutputParser (OutputParser))
import Myo.Output.Data.ParsedOutput (ParsedOutput)
import Myo.Output.Lang.Nix.Data.NixEvent (NixEvent (NixEvent))
import Myo.Output.Lang.Nix.Report (nixReport)
import Myo.Text.Parser.Combinators (anyTillChar, colon, minus, skipLine, tillEol, ws)

number :: TokenParsing m => Integer -> m Char -> m Integer
number base baseDigit =
  foldl' (\ x d -> base * x + toInteger (digitToInt d)) 0 <$> some baseDigit

path ::
  Monad m =>
  CharParsing m =>
  m Text
path = do
  slash <- char '/'
  rest <- toText <$> manyTill anyChar (try (choice [newline, colon]))
  pure (Text.cons slash rest)

normalize :: Integral a => a -> Int
normalize =
  fromIntegral . subtract 1

num ::
  TokenParsing m =>
  m Int
num =
  normalize <$> integer

lineCol ::
  TokenParsing m =>
  m (Int, Maybe Int)
lineCol =
  tuple line col
  where
    line =
      ws *> num <* ws <* colon
    col =
      Just <$> (ws *> num) <* skipOptional (ws *> minus *> ws *> num)

location ::
  Monad m =>
  TokenParsing m =>
  m Location
location = do
  p <- path
  (l, c) <- lineCol <* ws
  pure (Location p l c)

assertionLine ::
  TokenParsing m =>
  m ()
assertionLine =
  text "error:" *>
  ws *>
  anyTillChar ':' *>
  natural *>
  ws

traceEvent ::
  Monad m =>
  TokenParsing m =>
  m NixEvent
traceEvent =
  cons <$> msg <*> loc
  where
    cons m l =
      NixEvent l m
    msg =
      text "error:" *> ws *> tillEol
    loc =
      ws *> text "at" *> ws *> location

parseNixErrors ::
  Monad m =>
  CharParsing m =>
  TokenParsing m =>
  m (Vector NixEvent)
parseNixErrors =
  Vector.fromList . catMaybes <$> many (choice events <* skipMany newline)
  where
    events =
      [
        Just <$> traceEvent,
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

sanitizeNixOutput :: Text -> Text
sanitizeNixOutput =
  flip (foldl' @[] (flip searchReplaceAll)) regexes . Text.filter ('\b' /=)
  where
    regexes =
      [removeControlCharsRE]

parseNix :: Text -> Either OutputError ParsedOutput
parseNix =
  nixReport <=< mapLeft (OutputError.Parse . toText) . parseOnly parseNixErrors . sanitizeNixOutput

nixOutputParser :: OutputParser
nixOutputParser =
  OutputParser parseNix
