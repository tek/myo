module Myo.Command.Param where

import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.Map.Strict as Map
import Data.Some (Some (Some))

import Myo.Command.Data.Param (
  DefinedParam (DefinedParam, UndefinedParam),
  DefinedParams,
  OtherTag,
  ParamDefault (ParamDefault),
  ParamDefaults,
  ParamEnv (..),
  ParamId (..),
  ParamTag (..),
  ParamValue (..),
  ParamValues,
  otherParamTag,
  paramTagId,
  )
import qualified Myo.Command.Data.TemplateError as TemplateError
import Myo.Command.Data.TemplateError (TemplateError)
import Myo.Command.Optparse (OptparseArgs, optparseParams)

toDefinedParam :: ParamId -> ParamValue -> DSum ParamTag DefinedParam
toDefinedParam (ParamId pid) = \case
  ParamValue v -> ParamText pid :=> DefinedParam v
  ParamFlag f -> ParamBool pid :=> DefinedParam f

toDefinedParams :: ParamValues -> DefinedParams
toDefinedParams values =
  DMap.fromList (uncurry toDefinedParam <$> Map.toList values)

toValue ::
  (∀ x . ParamTag x -> Maybe x) ->
  ParamTag a ->
  DefinedParam a ->
  Maybe ParamValue
toValue undef t = \case
  DefinedParam a -> Just (mk a)
  UndefinedParam -> mk <$> undef t
  where
    mk = case t of
      ParamText _ -> ParamValue
      ParamBool _ -> ParamFlag

toValues ::
  (∀ x . ParamTag x -> Maybe x) ->
  DefinedParams ->
  ParamValues
toValues undef =
  Map.fromList .
  mapMaybe convert .
  DMap.toList
  where
    convert :: DSum ParamTag DefinedParam -> Maybe (ParamId, ParamValue)
    convert (key :=> value) = (paramTagId key,) <$> toValue undef key value

definedValues :: DefinedParams -> ParamValues
definedValues = toValues (const Nothing)

valuesOrEmpty :: DefinedParams -> ParamValues
valuesOrEmpty =
  toValues $ Just . \case
    ParamText _ -> ""
    ParamBool _ -> False

newParamEnv ::
  Member (Stop TemplateError) r =>
  ParamDefaults ->
  ParamValues ->
  Maybe OptparseArgs ->
  DefinedParams ->
  Sem r ParamEnv
newParamEnv defs overrides optparseArgs present = do
  optparseOverrides <- traverse (stopEitherWith TemplateError.Optparse . optparseParams present) optparseArgs
  pure ParamEnv {
    defaults = toDefinedParams (coerce defs),
    overrides = toDefinedParams overrides,
    cli = foldMap toDefinedParams optparseOverrides,
    resolved = mempty
  }

paramAlt ::
  Monad m =>
  m (DefinedParam a) ->
  m (DefinedParam a) ->
  m (DefinedParam a)
paramAlt left right =
  left >>= \case
    UndefinedParam -> right
    p -> pure p

resolveParam ::
  ∀ a r .
  Member (Stop TemplateError) r =>
  (∀ x . ParamTag x -> Sem r (DefinedParam x)) ->
  ParamEnv ->
  ParamTag a ->
  Sem r (DefinedParam a)
resolveParam resolveExternal ParamEnv {..} ptag = do
  paramAlt (lookupIn "CLI option" cli) $
    paramAlt (lookupIn "stored value" overrides) $
    paramAlt (resolveExternal ptag) $
    paramAlt (lookupIn "default" defaults) $
    pure UndefinedParam
  where
    lookupIn what params = do
      when (DMap.member other params) do
        stop (typeError what)
      pure (fromMaybe UndefinedParam (DMap.lookup ptag params))

    other :: ParamTag (OtherTag a)
    other = otherParamTag ptag

    typeError what = TemplateError.BadParamValue what (Some ptag) Nothing

resolveParamEnv ::
  ∀ r .
  Member (Stop TemplateError) r =>
  (∀ x . ParamTag x -> Sem r (DefinedParam x)) ->
  DefinedParams ->
  ParamEnv ->
  Sem r ParamEnv
resolveParamEnv resolveExternal active env = do
  resolved <- DMap.traverseWithKey resolveIfUndefined active
  pure env {resolved = env.resolved <> resolved}
  where
    resolveIfUndefined :: ∀ x . ParamTag x -> DefinedParam x -> Sem r (DefinedParam x)
    resolveIfUndefined ptag = \case
      UndefinedParam -> resolveParam resolveExternal env ptag
      DefinedParam a -> pure (DefinedParam a)
