module Myo.Command.CommandSpec.Resolve where

import Data.Some (Some (Some))
import Exon (exon)
import Ribosome (MsgpackDecode, Rpc, RpcError)
import Ribosome.Api (nvimCallFunction)
import Ribosome.Api.Variable (
  VariableError (VariableTypeMismatch),
  VariableName (VariableName),
  allScopes,
  findVariable,
  )
import qualified Ribosome.Host.Data.RpcError as RpcError

import Myo.Api.Function (functionExists)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.Param (DefinedParam (DefinedParam, UndefinedParam), ParamId (..), ParamTag, paramTagId)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.TemplateError as TemplateError
import Myo.Command.Data.TemplateError (TemplateError)
import Myo.Command.Override (OverrideResponse (ErrorResponse, FallbackResponse))
import Myo.Command.Param (paramAlt)

paramNames :: ParamId -> (Text, Text)
paramNames (ParamId pid) =
  ([exon|myo_param_#{pid}|], [exon|Myo_param_#{pid}|])

resultError :: Some ParamTag -> RpcError -> TemplateError
resultError ptag = \case
  e@(RpcError.Decode _) -> TemplateError.BadParamValue "value returned from the function" ptag (Just e)
  e -> TemplateError.ParamFunError ptag e

resolveParamFun ::
  MsgpackDecode a =>
  Members [Rpc !! RpcError, Stop RunError] r =>
  ParamTag a ->
  Text ->
  Sem r (DefinedParam a)
resolveParamFun ptag name = do
  resumeHoist RunError.Rpc (functionExists name) >>= \case
    True ->
      resumeHoist (RunError.Template . resultError (Some ptag)) (nvimCallFunction name []) >>= \case
        Right a -> pure (DefinedParam a)
        Left (ErrorResponse err) ->
          stop (RunError.Command (CommandError.User [exon|#{name} aborted: #{err}|]))
        Left (FallbackResponse _) ->
          pure UndefinedParam
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
      stop (RunError.Template (TemplateError.BadParamValue "variable" (Some ptag) Nothing))
    Left _ -> pure UndefinedParam
  where
    isTypeMismatch = \case
      VariableTypeMismatch -> True
      _ -> False

resolveParam ::
  ∀ a r .
  MsgpackDecode a =>
  Members [Rpc !! RpcError, Stop RunError] r =>
  ParamTag a ->
  Sem r (DefinedParam a)
resolveParam ptag =
  paramAlt (resolveParamVar ptag var) (resolveParamFun ptag fun)
  where
    (var, fun) = paramNames (paramTagId ptag)
