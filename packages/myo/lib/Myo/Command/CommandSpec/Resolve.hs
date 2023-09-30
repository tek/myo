module Myo.Command.CommandSpec.Resolve where

import qualified Data.Constraint.Extras as C
import qualified Data.Dependent.Map as DMap
import qualified Data.Map.Strict as Map
import Data.Some (Some (Some))
import Exon (exon)
import Ribosome (MsgpackDecode, Rpc, RpcError, toMsgpack)
import Ribosome.Api (nvimCallFunction)
import Ribosome.Api.Variable (
  VariableError (VariableTypeMismatch),
  VariableName (VariableName),
  allScopes,
  findVariable,
  )
import qualified Ribosome.Host.Data.RpcError as RpcError

import Myo.Command.Data.Param (
  DefinedParam (DefinedParam, UndefinedParam),
  DefinedParams,
  ParamDefault (..),
  ParamDefaults,
  ParamId (..),
  ParamTag,
  ParamValues,
  paramTagId,
  toDefinedParam,
  )
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)

paramNames :: ParamId -> (Text, Text)
paramNames (ParamId pid) =
  ([exon|myo_param_#{pid}|], [exon|Myo_param_#{pid}|])

resultError :: Some ParamTag -> RpcError -> RunError
resultError ptag = \case
  e@(RpcError.Decode _) -> RunError.BadParamValue ptag (Just e)
  e -> RunError.ParamFunError ptag e

resolveParamFun ::
  MsgpackDecode a =>
  Members [Rpc !! RpcError, Stop RunError] r =>
  ParamTag a ->
  Text ->
  Sem r (DefinedParam a)
resolveParamFun ptag name = do
  resumeHoist RunError.Rpc (nvimCallFunction "exists" [toMsgpack [exon|*#{name}|]]) >>= \case
    True -> DefinedParam <$> resumeHoist (resultError (Some ptag)) (nvimCallFunction name [])
    False -> pure UndefinedParam

resolveParamVar ::
  MsgpackDecode a =>
  Members [Rpc !! RpcError, Stop RunError] r =>
  ParamTag a ->
  Text ->
  Sem r (DefinedParam a)
resolveParamVar ptag name =
  findVariable (VariableName name) allScopes >>= \case
    Right a -> pure (DefinedParam a)
    Left errs | any isTypeMismatch errs ->
      stop (RunError.BadParamValue (Some ptag) Nothing)
    Left _ -> pure UndefinedParam
  where
    isTypeMismatch = \case
      VariableTypeMismatch -> True
      _ -> False

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
  MsgpackDecode a =>
  Members [Rpc !! RpcError, Stop RunError] r =>
  ParamDefaults ->
  ParamValues ->
  ParamTag a ->
  DefinedParam a ->
  Sem r (DefinedParam a)
resolveParam defs overrides ptag _ =
  paramAlt (check (Map.lookup pid overrides)) $
  paramAlt (resolveParamVar ptag var) $
  paramAlt (resolveParamFun ptag fun) $
  check (coerce (Map.lookup pid defs))
  where
    check =
      stopNote typeError . toDefinedParam ptag
    (var, fun) =
      paramNames pid
    pid =
      paramTagId ptag
    typeError =
      RunError.BadParamValue (Some ptag) Nothing

resolveParams ::
  Members [Rpc !! RpcError, Stop RunError] r =>
  ParamDefaults ->
  ParamValues ->
  DefinedParams ->
  Sem r DefinedParams
resolveParams defs overrides =
  DMap.traverseWithKey \ (ptag :: ParamTag x) (param :: DefinedParam x) ->
    C.has @MsgpackDecode ptag (resolveParam @x defs overrides ptag param)

resolveParamPure ::
  ParamValues ->
  ParamTag a ->
  DefinedParam a ->
  Either Text (DefinedParam a)
resolveParamPure params ptag _ =
  check (Map.lookup pid params)
  where
    check =
      maybeToRight typeError . toDefinedParam ptag
    typeError =
        [exon|Type mismatch for '##{pid}'|]
    pid =
      paramTagId ptag

resolveParamsPure ::
  ParamValues ->
  DefinedParams ->
  Either Text DefinedParams
resolveParamsPure overrides =
  DMap.traverseWithKey \ (ptag :: ParamTag x) (param :: DefinedParam x) ->
    resolveParamPure @x overrides ptag param
