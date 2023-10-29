module Myo.Output.Lang.Nix.Report where

import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Text.Parser.Char (noneOf, oneOf)
import Text.Parser.Combinators (between, sepBy1, skipOptional)
import Text.Parser.Token (TokenParsing, whiteSpace)

import Myo.Output.Data.Location (Location (Location))
import Myo.Output.Data.OutputError (OutputError)
import Myo.Output.Data.OutputEvent (LangOutputEvent (LangOutputEvent), OutputEventMeta (OutputEventMeta))
import Myo.Output.Data.ParsedOutput (ParsedOutput (ParsedOutput))
import Myo.Output.Data.String (lineNumber)
import Myo.Output.Lang.Nix.Data.NixEvent (NixEvent (NixEvent))
import Myo.Output.Lang.Nix.Syntax (nixSyntax)
import Myo.Output.Lang.Report (parsedOutputCons)
import Myo.Text.Parser.Combinators (formatTypeExprToplevel, typeExpr)

data NixMessage =
  NixMessage Text
  deriving stock (Eq, Show)

lqs :: String
lqs =
  "‘`"
rqs :: String
rqs =
  "’'"

quoted ::
  TokenParsing m =>
  m a ->
  m a
quoted =
  between (oneOf lqs) (oneOf rqs)

qnames ::
  TokenParsing m =>
  m Char ->
  m () ->
  m [Text]
qnames wordChar separator =
  fmap toText <$> quoted (sepBy1 (some wordChar) separator)

qname :: TokenParsing m => m Text
qname =
  unwords <$> qnames (noneOf ('\n' : ' ' : rqs)) whiteSpace

qType ::
  TokenParsing m =>
  m Text
qType =
  formatTypeExprToplevel <$> quoted typeExpr

ws :: TokenParsing m => m ()
ws =
  skipOptional whiteSpace

formatMessage :: NixMessage -> [Text]
formatMessage (NixMessage msg) =
  [msg]

formatLocation :: Location -> Text
formatLocation (Location path line _) =
  unwords [toText path, lineNumber, show (line + 1)]

formatReportLine :: LangOutputEvent NixMessage -> Vector Text
formatReportLine (LangOutputEvent (OutputEventMeta (Just location) _) message) =
  Vector.fromList (formatLocation location : formatMessage message <> [""])
formatReportLine _ =
  mempty

eventReport :: NixEvent -> Either OutputError (LangOutputEvent NixMessage)
eventReport (NixEvent loc messageText) =
  Right (event (NixMessage messageText))
  where
    event =
      LangOutputEvent (OutputEventMeta (Just loc) 0)

nixReport :: Vector NixEvent -> Either OutputError ParsedOutput
nixReport events =
  ParsedOutput nixSyntax . parsedOutputCons formatReportLine <$> traverse eventReport events
