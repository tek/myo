module Myo.Output.Lang.Scala.Report where

import Data.Attoparsec.Text (parseOnly)
import qualified Data.List as List (unwords)
import qualified Data.Text as Text (splitAt)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Text.Parser.Char (CharParsing, char, noneOf)
import Text.Parser.Combinators (between, choice, many, sepBy1, skipOptional, some, try)
import Text.Parser.Token (TokenParsing, brackets, parens, whiteSpace)

import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputError (OutputError)
import Myo.Output.Data.OutputEvent (LangOutputEvent(LangOutputEvent), OutputEventMeta(OutputEventMeta))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.String (colMarker, lineNumber)
import Myo.Output.Lang.Report (parsedOutputCons)
import Myo.Output.Lang.Scala.Data.ScalaEvent (EventType, ScalaEvent(ScalaEvent))
import qualified Myo.Output.Lang.Scala.Data.ScalaEvent as EventType (EventType(..))
import Myo.Output.Lang.Scala.Syntax (foundMarker, reqMarker, scalaSyntax, separatorMarker)

data ScalaMessage =
  ScalaMessage {
    smError :: Text,
    smInfo :: [Text],
    smCodeIndent :: Int,
    smCode :: Text
  }
  deriving (Eq, Show)

qname :: TokenParsing m => m Text
qname =
  toText . List.unwords <$> between (char '‘') (char '’') (sepBy1 (some (noneOf "\n ’")) whiteSpace)

ws :: TokenParsing m => m ()
ws =
  skipOptional whiteSpace

data ScalaOutputEvent =
  ScalaOutputEvent {
    level :: Int,
    index :: Int,
    location :: Location,
    message :: ScalaMessage
    }
  deriving (Eq, Show)

formatLocation :: Location -> Text
formatLocation (Location path line _) =
  unwords [toText path, lineNumber, show (line + 1)]

data FRExpr =
  Plain Text
  |
  Bracketed Text FRExpr
  |
  Parenthesized Text FRExpr
  |
  FR FRExpr FRExpr
  deriving (Eq, Show)

bracketParser ::
  Monad m =>
  CharParsing m =>
  TokenParsing m =>
  m FRExpr
bracketParser =
  choice $ try foundreq : surrounded ++ [Plain <$> plain]
  where
    surrounded = [try bracketed, try parenthesized]
    plain = toText <$> many (noneOf "[]()|")
    bracketed = do
      before <- plain
      sub <- brackets bracketParser
      return $ Bracketed before sub
    parenthesized = do
      before <- plain
      sub <- parens bracketParser
      return $ Parenthesized before sub
    foundreq =
      FR <$> part <* char '|' <*> part
      where
        part = choice $ surrounded ++ [Plain <$> plain]

formatInfo :: Text -> Text
formatInfo input =
  fromRight input (format <$> parse)
  where
    format (Plain text) = text
    format (Bracketed pre sub) =
      pre <> "[" <> format sub <> "]"
    format (Parenthesized pre sub) =
      pre <> "(" <> format sub <> ")"
    format (FR found req) =
      foundMarker <> format found <> separatorMarker <> "|" <> format req <> reqMarker
    parse = parseOnly bracketParser input

indent :: Text -> Text
indent =
  ("  " <>)

formatCode :: Location -> Int -> Text -> Text
formatCode (Location _ _ col) codeIndent code =
  pre <> colMarker <> post
  where
    num = maybe 0 (subtract codeIndent) col
    (pre, post) = Text.splitAt num code

formatReportLine :: LangOutputEvent ScalaMessage -> Vector Text
formatReportLine (LangOutputEvent (OutputEventMeta (Just location) _) (ScalaMessage err info codeIndent code)) =
  Vector.fromList lines'
  where
    lines' =
      formatLocation location : err : formattedInfo <> [formattedCode, ""]
    formattedInfo =
      indent . formatInfo <$> info
    formattedCode =
      indent $ formatCode location codeIndent code
formatReportLine _ =
  mempty

eventLevel :: EventType -> Int
eventLevel EventType.Warning = 1
eventLevel EventType.Error = 0

eventReport :: ScalaEvent -> LangOutputEvent ScalaMessage
eventReport (ScalaEvent loc tpe msg info codeIndent code) =
  LangOutputEvent (OutputEventMeta (Just loc) (eventLevel tpe)) (ScalaMessage msg info codeIndent code)

scalaReport :: Vector ScalaEvent -> Either OutputError ParsedOutput
scalaReport =
  Right . ParsedOutput scalaSyntax . parsedOutputCons formatReportLine . fmap eventReport
