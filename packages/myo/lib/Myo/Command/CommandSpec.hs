module Myo.Command.CommandSpec where

import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Exon (exon)
import qualified Log
import Ribosome (Rpc, RpcError)

import Myo.Command.CommandSpec.Resolve (paramNames, resolveParams, resolveParamsPure)
import qualified Myo.Command.Data.Command
import Myo.Command.Data.Command (Command (Command))
import Myo.Command.Data.CommandSpec (CommandSpec (..))
import Myo.Command.Data.CommandTemplate (CommandSegment (..), CommandTemplate (..), ParamSegment (..))
import Myo.Command.Data.Param (
  DefinedParam (DefinedParam, UndefinedParam),
  DefinedParams,
  ParamDefault (ParamDefault),
  ParamDefaults,
  ParamId (..),
  ParamTag (ParamBool, ParamText),
  ParamValue (ParamFlag, ParamValue),
  ParamValues,
  paramTagId,
  paramTagName,
  renderParamValue,
  )
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Optparse (OptparseArgs, optparseParams)

foldParams ::
  ∀ m a .
  Monad m =>
  Monoid a =>
  (Text -> m a) ->
  (∀ x . m a -> ParamTag x -> ParamSegment x -> m a) ->
  NonEmpty CommandSegment ->
  m a
foldParams lit f =
  segments
  where
    segments :: NonEmpty CommandSegment -> m a
    segments = fmap fold . traverse segment

    segment = \case
      SegmentLit t -> lit t
      SegmentParam pid sub -> f (paramSub sub) pid sub

    paramSub :: ParamSegment x -> m a
    paramSub = \case
      ParamRequired -> pure mempty
      ParamOptional -> pure mempty
      ParamTemplate tpl -> segments tpl
      ParamFlagTemplate tpl -> segments tpl

collectParams :: NonEmpty CommandSegment -> DefinedParams
collectParams =
  DMap.fromList . runIdentity . foldParams (pure mempty) \ res (paramTagName -> pid) sub ->
    pure (param pid sub : runIdentity res)
  where
    param pid = \case
      ParamFlagTemplate _ -> ParamBool pid :=> UndefinedParam
      _ -> ParamText pid :=> UndefinedParam

data ParamTask =
  ParamTask {
    pid :: ParamId,
    var :: Text,
    fun :: Text
  }

compileParam ::
  ∀ x m .
  Monad m =>
  (∀ y . ParamTag y -> m (DefinedParam y)) ->
  (∀ y a . ParamTag y -> m a) ->
  m Text ->
  ParamTag x ->
  ParamSegment x ->
  m Text
compileParam lookup noParam sub ptag seg = do
  p <- lookup ptag
  case (seg, p) of
    (ParamRequired, UndefinedParam) -> noParam ptag
    (ParamRequired, DefinedParam t) -> pure t
    (ParamOptional, UndefinedParam) -> pure ""
    (ParamOptional, DefinedParam t) -> pure t
    (ParamTemplate _, UndefinedParam) -> pure ""
    (ParamTemplate _, DefinedParam _) -> sub
    (ParamFlagTemplate _, UndefinedParam) -> pure ""
    (ParamFlagTemplate _, DefinedParam True) -> sub
    (ParamFlagTemplate _, DefinedParam False) -> pure ""

toValues :: DefinedParams -> Map ParamId ParamValue
toValues ps =
  Map.fromList $ catMaybes $ DMap.toList ps <&> \case
    _ :=> UndefinedParam -> Nothing
    ParamText pid :=> DefinedParam value -> Just (ParamId pid, ParamValue value)
    ParamBool pid :=> DefinedParam value -> Just (ParamId pid, ParamFlag value)

internalError ::
  Member (Stop RunError) r =>
  Maybe a ->
  Sem r a
internalError =
  stopNote (RunError.Internal "Parameters inconsistent during command assembly")

logResult ::
  Member Log r =>
  [Text] ->
  ParamDefaults ->
  ParamValues ->
  Maybe ParamValues ->
  ParamValues ->
  [Text] ->
  Sem r ()
logResult rendered params overrides optparseOverrides values cmdlines =
  traverse_ @[] Log.debug logLines
  where
    logLines =
      "Compiled command template:" : rendered ++
      renderValues "Defaults:" (coerce params) ++
      renderValues "Execution overrides:" overrides ++
      renderValues "Optparse overrides:" (fold optparseOverrides) ++
      renderValues "Final values:" values ++
      [[exon|Command lines: #{Text.unlines cmdlines}|]]
    renderValues :: Text -> ParamValues -> [Text]
    renderValues desc vs
      | Map.null vs
      = []
      | otherwise
      = desc : (uncurry renderValue <$> Map.toList vs)
    renderValue :: ParamId -> ParamValue -> Text
    renderValue k v = [exon|  ##{k}: #{renderParamValue v}|]

-- TODO collapse whitespace – a template usually contains multiple placeholders separated by whitespace, so if adjacent
-- ones evaluate to empty strings, we get multiple whitespaces
compileCommandSpec ::
  ∀ r .
  Members [Rpc !! RpcError, Stop RunError, Log] r =>
  ParamValues ->
  Maybe OptparseArgs ->
  CommandSpec ->
  Sem r (Map ParamId ParamValue, [Text])
compileCommandSpec overrides optparseArgs CommandSpec {template = CommandTemplate {rendered, segments}, params} = do
  optparseOverrides <- traverse (stopEitherWith RunError.Optparse . optparseParams present) optparseArgs
  definedParams <- resolveParams params (fold optparseOverrides <> overrides) present
  let
    lookup :: ∀ x . ParamTag x -> Sem r (DefinedParam x)
    lookup ptag = internalError (DMap.lookup ptag definedParams)
    values = toValues definedParams
  cmdlines <- traverse (foldParams pure (compileParam lookup noParam)) segments
  logResult rendered params overrides optparseOverrides values cmdlines
  pure (values, cmdlines)
  where
    present = foldMap collectParams segments

    noParam :: ParamTag x -> Sem r a
    noParam ptag =
      stop (RunError.NoParamValue pid var fun)
      where
        (var, fun) = paramNames pid
        pid = paramTagId ptag

compileTemplateWith :: CommandTemplate -> ParamValues -> Either Text [Text]
compileTemplateWith CommandTemplate {segments} params = do
  definedParams <- resolveParamsPure params present
  let
    lookup :: ∀ x . ParamTag x -> Either Text (DefinedParam x)
    lookup ptag = maybeToRight "Parameters inconsistent" (DMap.lookup ptag definedParams)
  traverse (foldParams pure (compileParam lookup noParam)) segments
  where
    present = foldMap collectParams segments

    noParam (paramTagName -> name) = Left [exon|No value for '#{name}'|]

compileTemplateWithDefaults :: Command -> Either Text [Text]
compileTemplateWithDefaults Command {cmdLines = CommandSpec {template, params}} =
  compileTemplateWith template (coerce params)
