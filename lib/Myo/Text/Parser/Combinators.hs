module Myo.Text.Parser.Combinators where

import Text.Parser.Char (CharParsing, char, newline, noneOf)
import Text.Parser.Combinators (manyTill, skipOptional)
import Text.Parser.Token (TokenParsing, whiteSpace)

colon :: CharParsing m => m Char
colon =
  char ':'

ws ::
  TokenParsing m =>
  m ()
ws =
  skipOptional whiteSpace

tillInLine ::
  CharParsing m =>
  m a ->
  m String
tillInLine =
  manyTill (noneOf ['\n'])

tillEol ::
  CharParsing m =>
  m String
tillEol =
  tillInLine newline

skipLine ::
  CharParsing m =>
  m ()
skipLine =
  void tillEol

emptyLine :: Monad m => CharParsing m => m ()
emptyLine =
  void $ newline *> newline
