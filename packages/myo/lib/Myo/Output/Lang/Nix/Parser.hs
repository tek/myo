module Myo.Output.Lang.Nix.Parser where

import Control.Lens (IndexedTraversal', index)
import Control.Lens.Regex.Text (Match, match)
import Data.Attoparsec.Text (parseOnly)
import Data.Char (digitToInt)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Path (parseRelFile, toFilePath, (</>))
import Path.IO (doesFileExist, getCurrentDir)
import Prelude hiding (try)
import Text.Parser.Char (CharParsing, anyChar, char, newline, text)
import Text.Parser.Combinators (choice, manyTill, skipMany, skipOptional, try)
import Text.Parser.Token (TokenParsing, integer, natural)

import Myo.Output.Data.Location (Location (Location))
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.OutputError as OutputError (OutputError (Parse))
import Myo.Output.Data.OutputEvent (OutputEvent (OutputEvent), OutputEventMeta (OutputEventMeta))
import Myo.Output.Data.OutputEvents (OutputEvents (OutputEvents))
import Myo.Output.Data.OutputParser (OutputParser (OutputParser))
import Myo.Output.Data.ParsedOutput (ParsedOutput (ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine (ReportLine))
import Myo.Output.Lang.Nix.Data.NixEvent (NixEvent (NixEvent))
import Myo.Output.Lang.Nix.Report (nixReport)
import Myo.Regex (regexML, removeControlCharsRE)
import Myo.Text.Parser.Combinators (anyTillChar, colon, minus, skipLine, tillEol, ws)
import Exon (exon)
import qualified Log

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
  (,) <$> line <*> col
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

sanitizeNixOutput :: Text -> Text
sanitizeNixOutput =
  flip (foldl' @[] \ z r -> z & r . match .~ "") regexes . Text.filter ('\b' /=)
  where
    regexes =
      [removeControlCharsRE]

parseNix ::
  Members [Stop OutputError, Log] r =>
  Text ->
  Sem r ParsedOutput
parseNix out = do
  Log.debug [exon|Parsing nix output: #{sanitized}|]
  parsed <- stopEitherWith (OutputError.Parse . toText) (parseOnly parseNixErrors sanitized)
  stopEither (nixReport parsed)
  where
    sanitized = sanitizeNixOutput out

storePathRE :: IndexedTraversal' Int Text Match
storePathRE =
  [regexML|^/nix/store/[^/]+/|]

adaptPath ::
  Member (Embed IO) r =>
  OutputEvent ->
  Sem r OutputEvent
adaptPath = \case
  OutputEvent (OutputEventMeta (Just (Location p line col)) level) ls -> do
    (newPath, newLines) <- case parseRelFile (toString (replaceStorePath p)) of
      Right localPath -> do
        cwd <- embed getCurrentDir
        exists <- doesFileExist (cwd </> localPath)
        pure (if exists then (toText (toFilePath localPath), sanitizeLine <$> ls) else (p, ls))
      Left _ ->
        pure (p, ls)
    pure (OutputEvent (OutputEventMeta (Just (Location newPath line col)) level) newLines)
  e ->
    pure e
  where
    sanitizeLine (ReportLine e t) =
      ReportLine e (replaceStorePath t)
    replaceStorePath =
      storePathRE . index 0 . match .~ ""

adaptPaths ::
  Member (Embed IO) r =>
  ParsedOutput ->
  Sem r ParsedOutput
adaptPaths (ParsedOutput syntax (OutputEvents events)) = do
  eventsWithLocalPaths <- traverse adaptPath events
  pure (ParsedOutput syntax (OutputEvents eventsWithLocalPaths))

nixOutputParser ::
  Members [Log, Embed IO] r =>
  OutputParser r
nixOutputParser =
  OutputParser (adaptPaths <=< parseNix)
