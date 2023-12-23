module Myo.Command.Optparse.Tokens where

import Data.Attoparsec.Text (Parser, parseOnly, runScanner)
import qualified Data.Text as Text
import Exon (exon)

data Reason =
  EndToken
  |
  LQuote Mode
  |
  RQuote
  deriving stock (Eq, Show, Generic)

data Mode =
  Single
  |
  Double
  |
  NoQuote
  |
  Abort Reason
  deriving stock (Eq, Show)

data S =
  S {
    mode :: Mode,
    escape :: Bool
  }
  deriving stock (Eq, Show, Generic)

dropTrailing :: Text -> Text
dropTrailing t0 =
  case Text.unsnoc t0 of
    Just (t, _) -> t
    Nothing -> t0

isQuote :: Char -> Maybe Mode
isQuote = \case
  '\'' -> Just Single
  '"' -> Just Double
  _ -> Nothing

pattern IsQuote :: Mode -> Char
pattern IsQuote m <- (isQuote -> Just m)

tokenize :: Text -> Mode -> Parser [Text]
tokenize prefix m0 =
  runScanner (S m0 False) step >>= \case
    (token, S _ True) -> fail [exon|Trailing backslash in args: #{toString (prefix <> token)}|]
    ("", S mode False) -> case mode of
      NoQuote
        | "" <- prefix
        -> pure []
        | otherwise
        -> pure [prefix]
      Abort _ -> fail "Internal error: Inconsistent parser state"
      _ -> unbalanced prefix
    (token, S mode False) -> do
      let
        new = prefix <> token
        next = dropTrailing new
      case mode of
        NoQuote -> pure [new]
        Abort EndToken -> do
          rest <- tokenize "" NoQuote
          pure (next : rest)
        Abort (LQuote newMode) ->
          tokenize next newMode
        Abort RQuote ->
          tokenize next NoQuote
        Single ->
          unbalanced new
        Double ->
          unbalanced new
  where
    step (S (Abort _) _) _ = Nothing

    step (S mode True) _ = Just (S mode False)

    step (S mode False) c = Just (S (step1 mode c) False)

    step1 NoQuote (IsQuote mode) = Abort (LQuote mode)

    step1 mode (IsQuote quote) | mode == quote = Abort RQuote

    step1 NoQuote ' ' = Abort EndToken

    step1 mode _ = mode

    unbalanced token = fail [exon|Unbalanced quotes in args: #{toString token}|]

optparseTokens :: Text -> Either Text [String]
optparseTokens =
  bimap toText (fmap toString) . parseOnly (tokenize "" NoQuote)
