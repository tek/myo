module Myo.Text.Parser.Combinators where

import Data.Char (isAlphaNum)
import qualified Data.Text as Text
import Exon (exon)
import Prelude hiding (try)
import Text.Parser.Char (CharParsing, anyChar, char, noneOf, oneOf, satisfy)
import Text.Parser.Combinators (choice, manyTill, skipOptional, try)
import Text.Parser.Token (TokenParsing (token), brackets, parens, whiteSpace)

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

lineBreakChars :: String
lineBreakChars = "\n\r"

lineBreak ::
  CharParsing m =>
  m ()
lineBreak = void (oneOf lineBreakChars)

notLineBreak ::
  CharParsing m =>
  m Char
notLineBreak = noneOf lineBreakChars

tillInLine ::
  CharParsing m =>
  m a ->
  m Text
tillInLine skip =
  toText <$> manyTill (noneOf lineBreakChars) skip

tillEol ::
  CharParsing m =>
  m Text
tillEol =
  tillInLine lineBreak

skipLine ::
  CharParsing m =>
  m ()
skipLine =
  void tillEol

emptyLine :: Monad m => CharParsing m => m ()
emptyLine =
  void $ lineBreak *> many (char ' ') *> lineBreak

identifier :: CharParsing m => m Char
identifier =
  satisfy \ c -> isAlphaNum c || c == '_' || c == '\'' || c == '.'

data Expr =
  Plain [Text]
  |
  Parenthesized [Text] Expr
  deriving stock (Eq, Show)

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
    plain = fmap toText <$> many (some identifier <* ws)
    parenthesized =
      Parenthesized <$> plain <*> parens parensExpr

data Aexp =
  AexpId Text
  |
  AexpParens [Aexp]
  deriving stock (Eq, Show)

aexp ::
  TokenParsing m =>
  m Aexp
aexp =
  token (choice [try parenthesized, plain])
  where
    plain = AexpId . toText <$> some identifier
    parenthesized =
      AexpParens <$> parens (many aexp)

formatAexp :: Aexp -> Text
formatAexp = \case
  AexpId t -> t
  AexpParens sub ->
    [exon|(#{Text.unwords (formatAexp <$> sub)})|]

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
wordNot cs =
  toText <$> many (noneOf $ " \n" <> toString cs) <* ws

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
  deriving stock (Eq, Show)

formatTypeParens :: Text -> Text
formatTypeParens sub =
  "(" <> sub <> ")"

formatTypeBrackets :: Text -> Text
formatTypeBrackets sub =
  "[" <> sub <> "]"

unqualify :: Text -> Text
unqualify name =
  fromMaybe name (last (Text.splitOn "." name))

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
      Single . toText <$> some (noneOf " ()[]\n\r‘'’")
    pars =
      Parens <$> parens typeExpr
    bracks =
      Brackets <$> brackets typeExpr
