module Myo.Command.Interpreter.CommandLog where

import Conc (interpretAtomic)
import qualified Data.ByteString as ByteString
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq ((:<|)), (|>))
import Ribosome (HostError, SettingError, Settings, reportError)
import qualified Ribosome.Settings as Settings

import Myo.Command.Data.CommandOutput (CommandOutput (..), CurrentOutput (..), OutputChunks (..), currentEmpty)
import Myo.Command.Effect.CommandLog (CommandLog (All, Append, Archive, ArchiveAll, Get, GetPrev, Set))
import Myo.Data.CommandId (CommandId)
import qualified Myo.Settings as Settings

truncOutputChunks ::
  Int ->
  OutputChunks ->
  OutputChunks
truncOutputChunks maxSize =
  spin
  where
    spin = \case
      OutputChunks {chunks = h :<| t, size} | size > maxSize ->
        spin OutputChunks { chunks = t, size = size - ByteString.length h }
      c ->
        c

trunc1 ::
  Int ->
  CommandOutput ->
  CommandOutput
trunc1 maxSize =
  #current %~ \case
    Unbuilt c -> Unbuilt (truncOutputChunks maxSize c)
    PartiallyBuilt c t -> PartiallyBuilt (truncOutputChunks maxSize c) t
    Built t -> Built t

appendChunk ::
  ByteString ->
  OutputChunks ->
  OutputChunks
appendChunk chunk OutputChunks {..} =
  OutputChunks (chunks |> chunk) (size + len)
  where
    len =
      ByteString.length chunk

appendCurrent ::
  ByteString ->
  CurrentOutput ->
  CurrentOutput
appendCurrent chunk = \case
  Unbuilt c ->
    Unbuilt (appendChunk chunk c)
  PartiallyBuilt c t ->
    PartiallyBuilt (appendChunk chunk c) t
  Built t ->
    PartiallyBuilt (appendChunk chunk def) t

append ::
  Int ->
  ByteString ->
  Maybe CommandOutput ->
  CommandOutput
append maxSize chunk = \case
  Just CommandOutput {..} ->
    trunc1 maxSize (CommandOutput prev (appendCurrent chunk current))
  Nothing ->
    CommandOutput Nothing (Unbuilt (OutputChunks (pure chunk) len))
  where
    len =
      ByteString.length chunk

builder :: OutputChunks -> Builder
builder =
  foldMap byteString . chunks

build :: Builder -> Text
build =
  decodeUtf8 . toLazyByteString

buildChunks :: OutputChunks -> Text
buildChunks =
  build . builder

buildCurrent :: CommandOutput -> (CommandOutput, Text)
buildCurrent CommandOutput {current, ..} =
  (CommandOutput {current = Built new, ..}, new)
  where
    new =
      case current of
        Unbuilt c -> buildChunks c
        PartiallyBuilt c t -> buildChunks c <> t
        Built t -> t

buildPrev :: CommandOutput -> Maybe Text
buildPrev =
  fmap (either id build) . prev

currentToPrev :: CurrentOutput -> Either Text Builder
currentToPrev = \case
  Unbuilt s -> Right (builder s)
  Built t -> Left t
  PartiallyBuilt s t -> Left (t <> buildChunks s)

archive :: CommandOutput -> CommandOutput
archive CommandOutput {..}
  | currentEmpty current =
    CommandOutput {..}
  | otherwise =
    CommandOutput {
      prev = Just (currentToPrev current),
      current = def
    }

setCurrent :: Text -> CommandOutput -> CommandOutput
setCurrent text CommandOutput {..} =
  CommandOutput {current = Built text, ..}

buildAndGet :: Maybe CommandOutput -> (Maybe Text, Maybe CommandOutput)
buildAndGet =
  fmap buildCurrent >>> \case
  Nothing -> (Nothing, Nothing)
  Just (co, t) -> (Just t, Just co)

buildAndGetAccum :: Map CommandId Text -> CommandId -> CommandOutput -> (Map CommandId Text, CommandOutput)
buildAndGetAccum acc ident co =
  (Map.insert ident built acc, newCo)
  where
    (newCo, built) =
      buildCurrent co

interpretCommandLog ::
  Member (Embed IO) r =>
  Sem r Int ->
  InterpreterFor CommandLog r
interpretCommandLog maxSize =
  interpretAtomic (mempty :: Map CommandId CommandOutput) .
  reinterpret \case
    Set ident text ->
      atomicModify' (Map.alter (Just . setCurrent text . fromMaybe def) ident . Map.adjust archive ident)
    Append ident chunk -> do
      size <- raise maxSize
      atomicModify' (Map.alter (Just . append size chunk) ident)
    Archive ident ->
      atomicModify' (Map.adjust archive ident)
    ArchiveAll ->
      atomicModify' (fmap archive)
    Get ident ->
      atomicState' \ s -> swap (Map.alterF buildAndGet ident s)
    GetPrev ident ->
      atomicGets (buildPrev <=< Map.lookup ident)
    All ->
      atomicState' (swap . Map.mapAccumWithKey buildAndGetAccum mempty)

maxSizeSetting ::
  Members [Settings !! SettingError, DataLog HostError] r =>
  Sem r Int
maxSizeSetting =
  Settings.get Settings.maxLogSize !! \ e -> 10000 <$ reportError (Just "command-log") e

interpretCommandLogSetting ::
  Members [Settings !! SettingError, DataLog HostError, Embed IO] r =>
  InterpreterFor CommandLog r
interpretCommandLogSetting =
  interpretCommandLog maxSizeSetting
