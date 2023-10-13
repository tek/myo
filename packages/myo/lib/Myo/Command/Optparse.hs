module Myo.Command.Optparse where

import qualified Control.Monad.Trans.State.Strict as Mtl
import Control.Monad.Trans.State.Strict (state)
import qualified Data.Dependent.Map as DMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Some (Some (Some))
import qualified Data.Text as Text
import qualified Options.Applicative as Optparse
import Options.Applicative (
  HasName,
  Parser,
  defaultPrefs,
  execParserPure,
  flag,
  info,
  long,
  renderFailure,
  short,
  strOption,
  )
import Ribosome (Args (Args))

import Myo.Command.Data.Param (
  DefinedParams,
  ParamId (ParamId),
  ParamTag (ParamBool, ParamText),
  ParamValue (ParamFlag, ParamValue),
  ParamValues,
  )
import Myo.Command.Optparse.Tokens (optparseTokens)

newtype OptparseArgs =
  OptparseArgs Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

fromArgs :: Args -> Maybe OptparseArgs
fromArgs (Args args) =
  OptparseArgs <$> justIf (not (Text.null args)) args

takeShortChar :: Text -> Mtl.State (Set Char) (Maybe Char)
takeShortChar =
  Text.uncons >>> \case
    Just (c, _) ->
      state \ used ->
        if | Set.member c used -> (Just c, Set.insert c used)
           | otherwise -> (Nothing, used)
    _ -> pure Nothing

paramMods ::
  HasName f =>
  Text ->
  Mtl.State (Set Char) (Optparse.Mod f a)
paramMods key =
  takeShortChar key <&> \ shortChar -> do
    long (toString key) <> foldMap short shortChar

paramParser :: Some ParamTag -> Mtl.State (Set Char) (Parser (Maybe (ParamId, ParamValue)))
paramParser (Some (ParamText key)) = do
  paramMods key <&> \ mods -> do
    raw <- optional (strOption mods)
    pure do
      value <- raw
      pure (ParamId key, ParamValue value)
paramParser (Some (ParamBool key)) = do
  paramMods key <&> \ mods -> do
    raw <- flag Nothing (Just True) mods
    pure do
      value <- raw
      pure (ParamId key, ParamFlag value)

paramsParser :: DefinedParams -> Parser ParamValues
paramsParser present =
  flip Mtl.evalState mempty do
    fmap (Map.fromList . catMaybes) . sequenceA <$> traverse paramParser (DMap.keys present)

optparseParams :: DefinedParams -> OptparseArgs -> Either Text ParamValues
optparseParams present (OptparseArgs args) = do
  tokens <- optparseTokens args
  result (execParserPure defaultPrefs (info (paramsParser present) mempty) tokens)
  where
    result = \case
      Optparse.Success a -> Right a
      Optparse.Failure e -> Left (toText (fst (renderFailure e "Myo")))
      Optparse.CompletionInvoked _ -> Left "Internal optparse error"
