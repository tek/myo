module Myo.Text.Parser.Combinators where

import Text.Parser.Char (CharParsing, anyChar, char, newline, noneOf, string)
import Text.Parser.Combinators (choice, eof, many, manyTill, skipMany, skipOptional, try)
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)
import Text.Parser.Token (TokenParsing, brackets, natural, whiteSpace)

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
  m [Char]
tillInLine =
  manyTill (noneOf ['\n'])

tillEol ::
  CharParsing m =>
  m [Char]
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
