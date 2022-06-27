module Myo.Command.Interpreter.CommandLog where

import Chiasma.Data.Ident (Ident)
import Conc (interpretAtomic)
import qualified Data.ByteString as ByteString
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq ((:<|)), (|>))

import Myo.Command.Data.CommandOutput (CommandOutput (..))
import Myo.Command.Effect.CommandLog (CommandLog (Append, Archive, Chunks, Get, GetPrev))

trunc1 :: Int -> CommandOutput -> CommandOutput
trunc1 maxSize =
  spin
  where
    spin = \case
      CommandOutput {current = h :<| t, ..} | currentSize > maxSize ->
        spin CommandOutput {
          current = t,
          currentSize = currentSize - ByteString.length h,
          ..
        }
      c ->
        c

append ::
  Int ->
  ByteString ->
  Maybe CommandOutput ->
  CommandOutput
append maxSize chunk = \case
  Just CommandOutput {..} ->
    trunc1 maxSize (CommandOutput prev Nothing (current |> chunk) (currentSize + len))
  Nothing ->
    CommandOutput Nothing Nothing (pure chunk) len
  where
    len =
      ByteString.length chunk

builder :: Seq ByteString -> Builder
builder =
  foldMap byteString

build :: Builder -> Text
build =
  decodeUtf8 . toLazyByteString

buildCurrent :: CommandOutput -> CommandOutput
buildCurrent old =
  old { currentBuilt = Just (build (builder (current old))) }

buildPrev :: CommandOutput -> Maybe Text
buildPrev =
  fmap (either id build) . prev

archive :: CommandOutput -> CommandOutput
archive CommandOutput {..} =
  CommandOutput {
    prev = Just (maybe (Right (builder current)) Left currentBuilt),
    currentBuilt = Nothing,
    current = mempty,
    currentSize = 0
  }

interpretCommandLog ::
  Member (Embed IO) r =>
  Int ->
  InterpreterFor CommandLog r
interpretCommandLog maxSize =
  interpretAtomic (mempty :: Map Ident CommandOutput) .
  reinterpret \case
    Append ident chunk ->
      atomicModify' \ s ->
        Map.alter (Just . append maxSize chunk) ident s
    Archive ident ->
      atomicModify' (Map.adjust archive ident)
    Chunks ident ->
      atomicGets (fmap current . Map.lookup ident)
    Get ident ->
      atomicState' \ s ->
        let new = Map.adjust buildCurrent ident s
        in (new, Map.lookup ident new >>= currentBuilt)
    GetPrev ident ->
      atomicGets (buildPrev <=< Map.lookup ident)
