module Myo.Output.Lang.Haskell.Report where

import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text as Text (intercalate, lines)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Text.Parser.Char (CharParsing, anyChar, noneOf, oneOf, string, char)
import Text.Parser.Combinators (between, choice, manyTill, sepBy1, skipMany, skipOptional, try)
import Text.Parser.Token (TokenParsing, parens, token, whiteSpace)

import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.OutputError as OutputError (OutputError(Parse))
import Myo.Output.Data.OutputEvent (LangOutputEvent(LangOutputEvent), OutputEventMeta(OutputEventMeta))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.String (lineNumber)
import Myo.Output.Lang.Haskell.Data.HaskellEvent (EventType, HaskellEvent(HaskellEvent))
import qualified Myo.Output.Lang.Haskell.Data.HaskellEvent as EventType (EventType(..))
import Myo.Output.Lang.Haskell.Syntax (
  ambiguousTypeVarMarker,
  dataCtorNotInScopeMarker,
  doResDiscardMarker,
  foundReqMarker,
  haskellSyntax,
  invalidImportNameMarker,
  invalidQualifiedNameMarker,
  moduleImportMarker,
  moduleNameMismatchMarker,
  nameImportsMarker,
  patternsMarker,
  runtimeErrorMarker,
  unknownModuleMarker, kindMismatchMarker
  )
import Myo.Output.Lang.Report (parsedOutputCons)
import Myo.Text.Parser.Combinators (
  parensExpr,
  unParens,
  formatExpr,
  typeExpr,
  formatTypeExpr,
  formatTypeExprToplevel,
  TypeExpr,
  )
import qualified Myo.Text.Parser.Combinators as Expr
import Prelude hiding (try)
import Exon (exon)

data HaskellMessage =
  FoundReq1 Text Text
  |
  FoundReq2 Text Text
  |
  TypeNotInScope Text
  |
  VariableNotInScope Text Text
  |
  Verbatim Text
  |
  NoMethod Text
  |
  ModuleImport Text
  |
  NamesImport Text [Text]
  |
  ParseError
  |
  NoInstance Text Text
  |
  DoNotationResultDiscarded Text
  |
  InvalidImportName Text Text
  |
  ModuleNameMismatch Text Text
  |
  NoSuchModule Text
  |
  AmbiguousTypeVar Text Text Text
  |
  InvalidQualifiedName Text
  |
  RuntimeError Text
  |
  NonExhaustivePatterns Text
  |
  DataCtorNotInScope Text
  |
  KindMismatch Text Text
  |
  PolysemyEffect { effect :: Text, resumableError :: Maybe Text }
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

foundReq1 ::
  Applicative m =>
  TokenParsing m =>
  m HaskellMessage
foundReq1 =
  flip FoundReq1 <$> req <*> found
  where
    req = string "Couldn't match expected type" *> ws *> qType <* ws
    found = string "with actual type" *> ws *> qType

foundReq2 ::
  TokenParsing m =>
  m HaskellMessage
foundReq2 =
  FoundReq2 <$> found <*> req <* skipMany anyChar
  where
    found =
      string "Couldn't match type " *> qType
    req =
      ws *> string "with" *> ws *> skipOptional (string "actual type") *> ws *> qType

typeNotInScope ::
  TokenParsing m =>
  m HaskellMessage
typeNotInScope =
  string "Not in scope: type constructor or class" *> ws *> (TypeNotInScope <$> qname)

variableNotInScope ::
  Monad m =>
  TokenParsing m =>
  m HaskellMessage
variableNotInScope =
  ws *> string "Variable not in scope:" *> ws *> (VariableNotInScope <$> name <*> tpe)
  where
    name =
      toText <$> manyTill anyChar (try (ws *> string "::"))
    tpe =
      toText <$> (ws *> many (noneOf "\n"))

noMethod ::
  TokenParsing m =>
  m HaskellMessage
noMethod =
  string "No explicit implementation for" *> ws *> (NoMethod <$> qname) <* skipMany anyChar

importIntro ::
  TokenParsing m =>
  m ()
importIntro =
  token (string "The") *> skipOptional (token $ string "qualified") *> token (string "import of") *> ws

moduleImport ::
  TokenParsing m =>
  m HaskellMessage
moduleImport =
  ModuleImport <$> (importIntro *> qname <* post)
  where
    post =
      ws <* string "is redundant"

namesImport ::
  TokenParsing m =>
  m HaskellMessage
namesImport =
  flip NamesImport <$> names <*> module'
  where
    names = importIntro *> qnames (noneOf "\n, ’") (void $ many $ oneOf "\n, \t") <* ws
    module' = string "from module" *> ws *> qname <* ws <* string "is redundant"

parseError ::
  TokenParsing m =>
  m HaskellMessage
parseError =
  ParseError <$ (string "Parse error:" *> many anyChar)

noInstance1 ::
  TokenParsing m =>
  m HaskellMessage
noInstance1 =
  NoInstance . formatExpr <$> (string "No instance for" *> ws *> parens parensExpr) <*> (many (noneOf lqs) *> qType)

noInstance2 ::
  TokenParsing m =>
  m HaskellMessage
noInstance2 =
  NoInstance . formatExpr <$> (string "Could not deduce" *> ws *> parens parensExpr) <*> name
  where
    name =
      choice [string "arising from a use of", string "arising from the literal"] *> ws *> qname

doNotationResultDiscarded ::
  TokenParsing m =>
  m HaskellMessage
doNotationResultDiscarded =
  DoNotationResultDiscarded <$> (string "A do-notation statement discarded a result of type" *> ws *> qType)

invalidImportName ::
  TokenParsing m =>
  m HaskellMessage
invalidImportName =
  InvalidImportName <$> m <*> export
  where
    m =
      string "Module" *> ws *> qname
    export =
      ws *> string "does not export" *> ws *> qname

moduleNameMismatch ::
  TokenParsing m =>
  m HaskellMessage
moduleNameMismatch =
  ModuleNameMismatch <$> saw <*> expected
  where
    saw =
      string "File name does not match module name:" *> ws *> string "Saw:" *> ws *> qname
    expected =
      ws *> string "Expected:" *> ws *> qname

unknownModule ::
  TokenParsing m =>
  m HaskellMessage
unknownModule =
  NoSuchModule <$> name
  where
    name =
      string "Could not find module" *> ws *> qname

ambiguousTypeVar ::
  TokenParsing m =>
  m HaskellMessage
ambiguousTypeVar =
  AmbiguousTypeVar <$> var <*> func <*> constraint
  where
    var =
      string "Ambiguous type variable" *> ws *> qname
    func =
      ws *> string "arising from a use of" *> ws *> qname
    constraint =
      ws *> string "prevents the constraint" *> ws *> (unParens . formatExpr <$> quoted parensExpr)

invalidQualifiedName ::
  TokenParsing m =>
  m HaskellMessage
invalidQualifiedName =
  InvalidQualifiedName <$> name
  where
    name =
      string "Not in scope:" *> ws *> qname

dataCtorNotInScope ::
  TokenParsing m =>
  m HaskellMessage
dataCtorNotInScope =
  DataCtorNotInScope <$> name
  where
    name =
      string "Not in scope: data constructor" *> ws *> qname

nonExhausivePatterns ::
  TokenParsing m =>
  m HaskellMessage
nonExhausivePatterns =
  NonExhaustivePatterns <$> name
  where
    name =
      string "Pattern match(es) are non-exhaustive" *> ws *> string "In an equation for" *> ws *> qname

kindMismatch ::
  Applicative m =>
  TokenParsing m =>
  m HaskellMessage
kindMismatch =
  flip KindMismatch <$> req <*> found
  where
    req = string "Expected kind" *> ws *> qname <* string "," <* ws
    found = string "but" *> ws *> qname *> string " has kind " *> qname

formatEffect :: TypeExpr -> HaskellMessage
formatEffect =
  cons . format
  where
    format = \case
      Expr.Apply [Expr.Single "Resumable", e, eff] ->
        (formatTypeExprToplevel eff, Just (formatTypeExpr e))
      Expr.Apply [eff, Expr.Single "!!", e] ->
        (formatTypeExprToplevel eff, Just (formatTypeExpr e))
      Expr.Apply (Expr.Single "Resumable" : e : rest) ->
        (Text.intercalate " " (formatTypeExpr <$> rest), Just (formatTypeExpr e))
      Expr.Parens par ->
        format par
      e ->
        (formatTypeExpr e, Nothing)
    cons (eff, res) =
      PolysemyEffect eff res

polysemyCouldNotDeduce ::
  TokenParsing m =>
  m HaskellMessage
polysemyCouldNotDeduce =
  formatEffect <$> (intro *> parens typeExpr)
  where
    intro =
      string "Could not deduce:" *> ws *> optional (string "Polysemy.Internal.Union.") *> string "LocateEffect" *> ws

polysemyUnhandled ::
  TokenParsing m =>
  m HaskellMessage
polysemyUnhandled =
  string "Unhandled effect" *> ws *> between (char '\'') (char '\'') (formatEffect <$> typeExpr)

polysemyAmbiguous ::
  TokenParsing m =>
  m HaskellMessage
polysemyAmbiguous =
  string "Ambiguous use of effect" *> ws *> between (char '\'') (char '\'') (formatEffect <$> typeExpr)

verbatim :: CharParsing m => m HaskellMessage
verbatim =
  Verbatim . toText <$> many anyChar

parseMessage ::
  TokenParsing m =>
  Monad m =>
  m HaskellMessage
parseMessage =
  choice msgs <* skipMany anyChar
  where
    msgs =
      [
        foundReq1,
        foundReq2,
        typeNotInScope,
        noMethod,
        moduleImport,
        namesImport,
        parseError,
        noInstance1,
        noInstance2,
        variableNotInScope,
        doNotationResultDiscarded,
        invalidImportName,
        moduleNameMismatch,
        unknownModule,
        ambiguousTypeVar,
        invalidQualifiedName,
        dataCtorNotInScope,
        nonExhausivePatterns,
        kindMismatch,
        polysemyCouldNotDeduce,
        polysemyUnhandled,
        polysemyAmbiguous,
        verbatim
      ]

formatMessage :: HaskellMessage -> [Text]
formatMessage (FoundReq1 found req) =
  [foundReqMarker, found, req]
formatMessage (FoundReq2 found req) =
  [foundReqMarker, found, req]
formatMessage (TypeNotInScope tpe) =
  ["type not in scope", tpe]
formatMessage (NoMethod meth) =
  ["method not implemented: " <> meth]
formatMessage (ModuleImport name) =
  [moduleImportMarker, name]
formatMessage (NamesImport module' names) =
  [nameImportsMarker, Text.intercalate ", " names,  module']
formatMessage ParseError =
  ["syntax error"]
formatMessage (NoInstance tpe trigger) =
  ["!instance: " <> trigger, tpe]
formatMessage (VariableNotInScope name tpe) =
  ["variable not in scope", name <> " :: " <> tpe]
formatMessage (DoNotationResultDiscarded tpe) =
  [doResDiscardMarker, tpe]
formatMessage (InvalidImportName m export) =
  [invalidImportNameMarker, export, m]
formatMessage (ModuleNameMismatch saw expected) =
  [moduleNameMismatchMarker, saw, expected]
formatMessage (NoSuchModule name) =
  [unknownModuleMarker, name]
formatMessage (AmbiguousTypeVar var func constraint) =
  [ambiguousTypeVarMarker, var, func, constraint]
formatMessage (InvalidQualifiedName name) =
  [invalidQualifiedNameMarker, name]
formatMessage (RuntimeError msg) =
  [runtimeErrorMarker, msg]
formatMessage (NonExhaustivePatterns fun) =
  [patternsMarker, fun]
formatMessage (DataCtorNotInScope name) =
  [dataCtorNotInScopeMarker, name]
formatMessage (Verbatim txt) =
  Text.lines txt
formatMessage (KindMismatch found req) =
  [kindMismatchMarker, found, req]
formatMessage (PolysemyEffect eff (Just err)) =
  [[exon|!effect: #{eff} !! #{err}|]]
formatMessage (PolysemyEffect eff Nothing) =
  [[exon|!effect: #{eff}|]]

formatLocation :: Location -> Text
formatLocation (Location path line _) =
  unwords [toText path, lineNumber, show (line + 1)]

formatReportLine :: LangOutputEvent HaskellMessage -> Vector Text
formatReportLine (LangOutputEvent (OutputEventMeta (Just location) _) message) =
  Vector.fromList (formatLocation location : formatMessage message <> [""])
formatReportLine _ =
  mempty

eventLevel :: EventType -> Int
eventLevel EventType.Warning = 1
eventLevel EventType.Error = 0
eventLevel EventType.RuntimeError = 0
eventLevel EventType.Patterns = 0

eventReport :: HaskellEvent -> Either OutputError (LangOutputEvent HaskellMessage)
eventReport (HaskellEvent loc tpe (messageText :| _)) =
  bimap outputError event (message tpe)
  where
    message EventType.RuntimeError =
      pure $ RuntimeError messageText
    message EventType.Patterns =
      pure $ NonExhaustivePatterns messageText
    message _ =
      parseOnly parseMessage messageText
    outputError =
      OutputError.Parse . toText
    event =
      LangOutputEvent (OutputEventMeta (Just loc) (eventLevel tpe))

haskellReport :: Vector HaskellEvent -> Either OutputError ParsedOutput
haskellReport =
  fmap (ParsedOutput haskellSyntax . parsedOutputCons formatReportLine) <$> traverse eventReport
