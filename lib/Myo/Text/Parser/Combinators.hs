module Myo.Text.Parser.Combinators where

import Data.Char (isSpace)
import Data.Text.Prettyprint.Doc (Pretty(..), nest, vsep, (<+>))
import Text.Parser.Char (CharParsing, alphaNum, char, newline, noneOf, satisfy)
import Text.Parser.Combinators (choice, manyTill, skipOptional, try)
import Text.Parser.Token (TokenParsing, brackets, parens, someSpace, token, whiteSpace)
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
