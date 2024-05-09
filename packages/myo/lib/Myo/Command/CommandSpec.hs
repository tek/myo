module Myo.Command.CommandSpec where

import qualified Data.Constraint.Extras as C
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.Map.Strict as Map
import Data.Some (Some (Some))
import qualified Data.Text as Text
import Exon (exon)
import qualified Log
import Polysemy (run)
import Ribosome (MsgpackDecode, Rpc, RpcError)

import Myo.Command.CommandSpec.Resolve (paramNames, resolveParam)
import qualified Myo.Command.Data.Command
import Myo.Command.Data.Command (Command (Command))
import Myo.Command.Data.CommandSpec (CommandSpec (..))
import Myo.Command.Data.CommandTemplate (
  CommandSegment (..),
  CommandTemplate (..),
  ParamSegment (..),
  parseCommandSegments,
  )
import qualified Myo.Command.Data.Param
import Myo.Command.Data.Param (
  DefinedParam (DefinedParam, UndefinedParam),
  DefinedParams,
  ParamDefault (ParamDefault),
  ParamDefaults,
  ParamEnv (ParamEnv),
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
import qualified Myo.Command.Data.TemplateError as TemplateError
import Myo.Command.Data.TemplateError (TemplateError)
import Myo.Command.Optparse (OptparseArgs)
import Myo.Command.Param (definedValues, newParamEnv, resolveParamEnv)

foldSegments ::
  ∀ m a .
  Monad m =>
  Monoid a =>
  (Text -> m a) ->
  (∀ x . m a -> ParamTag x -> ParamSegment x -> m a) ->
  NonEmpty CommandSegment ->
  m a
foldSegments lit seg =
  segments
  where
    segments :: NonEmpty CommandSegment -> m a
    segments = fmap fold . traverse segment

    segment = \case
      SegmentLit t -> lit t
      SegmentParam pid sub -> seg (paramSub sub) pid sub

    paramSub :: ParamSegment x -> m a
    paramSub = \case
      ParamRequired -> pure mempty
      ParamOptional -> pure mempty
      ParamTemplate tpl -> segments tpl
      ParamFlagTemplate tpl -> segments tpl

collectParams :: NonEmpty CommandSegment -> DefinedParams
collectParams =
  DMap.fromList . runIdentity . foldSegments (pure mempty) \ res (paramTagName -> pid) sub ->
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
  ∀ x r .
  (∀ y . ParamTag y -> Sem r (DefinedParam y)) ->
  (∀ y a . ParamTag y -> Sem r a) ->
  (∀ y . ParamTag y -> Text -> Sem r Text) ->
  Sem r Text ->
  ParamTag x ->
  ParamSegment x ->
  Sem r Text
compileParam lookup noParam dynamic sub ptag seg = do
  p <- lookup ptag
  case (seg, p) of
    (ParamRequired, UndefinedParam) -> noParam ptag
    (ParamRequired, DefinedParam t) -> dynamic ptag t
    (ParamOptional, UndefinedParam) -> pure ""
    (ParamOptional, DefinedParam t) -> dynamic ptag t
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
  Member (Stop TemplateError) r =>
  Maybe a ->
  Sem r a
internalError =
  stopNote (TemplateError.Internal "Parameters inconsistent during command assembly")

logResult ::
  Member Log r =>
  [Text] ->
  ParamEnv ->
  [Text] ->
  Sem r ()
logResult rendered ParamEnv {defaults, overrides, cli, resolved} cmdlines =
  traverse_ @[] Log.debug logLines
  where
    logLines =
      "Compiled command template:" : rendered ++
      renderValues "Defaults:" (coerce defaults) ++
      renderValues "Execution overrides:" overrides ++
      renderValues "Optparse overrides:" cli ++
      renderValues "Resolved values:" resolved ++
      [[exon|Command lines: #{Text.unlines cmdlines}|]]
    renderValues :: Text -> DefinedParams -> [Text]
    renderValues desc (definedValues -> vs)
      | Map.null vs
      = []
      | otherwise
      = desc : (uncurry renderValue <$> Map.toList vs)
    renderValue :: ParamId -> ParamValue -> Text
    renderValue k v = [exon|  ##{k}: #{renderParamValue v}|]

compileSegments ::
  ∀ r .
  Members [State ParamEnv, Stop TemplateError] r =>
  (∀ x . ParamTag x -> Sem r (DefinedParam x)) ->
  NonEmpty CommandSegment ->
  Sem r Text
compileSegments resolve =
  spin
  where
    spin segments =
      foldSegments pure (compileParam lookup noParam dynamic) segments
      where
        lookup :: ∀ x . ParamTag x -> Sem r (DefinedParam x)
        lookup ptag = do
          penv <- get
          internalError (DMap.lookup ptag penv.resolved)

        noParam :: ParamTag x -> Sem r a
        noParam ptag =
          stop (TemplateError.NoParamValue pid var fun)
          where
            (var, fun) = paramNames pid
            pid = paramTagId ptag

        dynamic :: ∀ x . ParamTag x -> Text -> Sem r Text
        dynamic ptag value = do
          let err = TemplateError.DynamicParseError (Some ptag)
          nestedSegments <- stopEitherWith err (parseCommandSegments value)
          let present = collectParams nestedSegments
          penv <- get
          newEnv <- resolveParamEnv resolve present penv
          put newEnv
          spin nestedSegments

-- TODO collapse whitespace – a template usually contains multiple placeholders separated by whitespace, so if adjacent
-- ones evaluate to empty strings, we get multiple whitespaces
compileCommandSpec ::
  ∀ r .
  Members [Rpc !! RpcError, Stop RunError, Log] r =>
  ParamValues ->
  Maybe OptparseArgs ->
  CommandSpec ->
  Sem r (ParamValues, [Text])
compileCommandSpec overrides optparseArgs CommandSpec {template = CommandTemplate {rendered, segments}, params} = do
  resolvedEnv <- templateError do
    initialEnv <- newParamEnv params overrides optparseArgs present
    resolveParamEnv resolve present initialEnv
  (finalEnv, cmdlines) <- templateError (runState resolvedEnv (traverse (compileSegments resolve) segments))
  logResult rendered finalEnv cmdlines
  pure (definedValues finalEnv.resolved, cmdlines)
  where
    present = foldMap collectParams segments
    resolve (ptag :: ParamTag x) = C.has @MsgpackDecode ptag (resolveParam @x ptag)
    templateError = mapStop RunError.Template

compileTemplateWith ::
  CommandTemplate ->
  ParamDefaults ->
  ParamValues ->
  Either TemplateError [Text]
compileTemplateWith CommandTemplate {segments} defs params = do
  run $ runStop do
    initialEnv <- newParamEnv defs params Nothing present
    resolvedEnv <- resolveParamEnv resolve present initialEnv
    -- definedParams <- stopEither (resolveParamsPure params present)
    evalState resolvedEnv (traverse (compileSegments resolve) segments)
  where
    present = foldMap collectParams segments
    resolve _ = pure UndefinedParam

compileTemplateWithDefaults :: Command -> Either TemplateError [Text]
compileTemplateWithDefaults Command {cmdLines = CommandSpec {template, params}} =
  compileTemplateWith template params (coerce params)
