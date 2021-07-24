module Myo.Text.Parser.Combinators where

import qualified Data.Text as Text (drop, dropEnd, intercalate, take, takeEnd, splitOn)
import Text.Parser.Char (CharParsing, alphaNum, anyChar, char, newline, noneOf)
import Text.Parser.Combinators (choice, manyTill, skipOptional, try)
import Text.Parser.Token (TokenParsing, parens, whiteSpace, brackets)

colon :: CharParsing m => m Char
colon =
  char ':'

minus :: CharParsing m => m Char
minus =
  char '-'

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
  fmap toText <$> manyTill (noneOf ['\n'])

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
  Plain [Text]
  |
  Parenthesized [Text] Expr
  deriving (Eq, Show)

formatParens :: [Text] -> Text -> Text
formatParens before sub =
  (Text.intercalate " " before) <> (if null before then "" else " ") <> "(" <> sub <> ")"

formatExpr :: Expr -> Text
formatExpr = \case
  Plain t ->
    Text.intercalate " " t
  Parenthesized before sub ->
    formatParens before (formatExpr sub)

formatExprToplevel :: Expr -> Text
formatExprToplevel = \case
  Parenthesized [] sub ->
    formatExpr sub
  expr ->
    formatExpr expr

parensExpr ::
  TokenParsing m =>
  m Expr
parensExpr =
  choice [try parenthesized, Plain <$> plain]
  where
    plain = fmap toText <$> many (some alphaNum <* ws)
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
  fmap toText <$> manyTill anyChar . char

data TypeExpr =
  Single Text
  |
  Apply [TypeExpr]
  |
  Parens TypeExpr
  |
  Brackets TypeExpr
  deriving (Eq, Show)

formatTypeParens :: Text -> Text
formatTypeParens sub =
  "(" <> sub <> ")"

formatTypeBrackets :: Text -> Text
formatTypeBrackets sub =
  "[" <> sub <> "]"

unqualify :: Text -> Text
unqualify name =
  maybe name last (nonEmpty ((Text.splitOn ".") name))

formatTypeExpr :: TypeExpr -> Text
formatTypeExpr = \case
  Single t ->
    unqualify t
  Apply t ->
    Text.intercalate " " (formatTypeExpr <$> t)
  Parens sub ->
    formatTypeParens (formatTypeExpr sub)
  Brackets sub ->
    formatTypeBrackets (formatTypeExpr sub)

formatTypeExprToplevel :: TypeExpr -> Text
formatTypeExprToplevel = \case
  Parens sub ->
    formatTypeExpr sub
  expr ->
    formatTypeExpr expr

typeExpr ::
  TokenParsing m =>
  m TypeExpr
typeExpr =
  Apply <$> many ((try pars <|> try bracks <|> plain) <* ws)
  where
    plain =
      Single . toText <$> some (noneOf " ()[]\n'")
    pars =
      Parens <$> parens typeExpr
    bracks =
      Brackets <$> brackets typeExpr
