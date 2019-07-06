module Myo.Text.Parser.Combinators where

import Data.Char (isSpace)
import qualified Data.Text as Text (drop, dropEnd, take, takeEnd)
import Text.Parser.Char (CharParsing, alphaNum, anyChar, char, newline, noneOf, satisfy)
import Text.Parser.Combinators (choice, manyTill, skipOptional, try)
import Text.Parser.Token (TokenParsing, parens, whiteSpace)
import qualified Text.Show

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
  m Text
tillInLine =
  toText <$$> manyTill (noneOf ['\n'])

tillEol ::
  CharParsing m =>
  m Text
tillEol =
  tillInLine newline

skipLine ::
  CharParsing m =>
  m ()
skipLine =
  void tillEol

emptyLine :: Monad m => CharParsing m => m ()
emptyLine =
  void $ newline *> many (char ' ') *> newline

data Expr =
  Plain Text
  |
  Parenthesized Text Expr
  deriving Eq

instance Text.Show.Show Expr where
  show (Plain t) =
    toString t
  show (Parenthesized before sub) =
    toString before <> "(" <> show sub <> ")"

parensExpr ::
  TokenParsing m =>
  m Expr
parensExpr =
  choice [try parenthesized, Plain <$> plain]
  where
    plain = toText <$> many (alphaNum <|> satisfy isSpace)
    parenthesized =
      Parenthesized <$> plain <*> parens parensExpr

unParens :: Text -> Text
unParens a =
  if hasParens then withoutParens a else a
  where
    hasParens =
      Text.take 1 a == "(" && Text.takeEnd 1 a ==")"
    withoutParens =
      Text.drop 1 . Text.dropEnd 1

word ::
  TokenParsing m =>
  m Text
word =
  toText <$> many (noneOf " \n") <* ws

wordNot ::
  TokenParsing m =>
  Text ->
  m Text
wordNot chars =
  toText <$> many (noneOf $ " " <> toString chars) <* ws

anyTillChar ::
  TokenParsing m =>
  Char ->
  m Text
anyTillChar =
  toText <$$> manyTill anyChar . char
