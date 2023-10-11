module Myo.Api.Function where

import Data.MessagePack (Object)
import Exon (exon)
import Ribosome (MsgpackDecode, Rpc, toMsgpack)
import Ribosome.Api (nvimCallFunction)

functionExists ::
  Member Rpc r =>
  Text ->
  Sem r Bool
functionExists name =
  nvimCallFunction "exists" [toMsgpack [exon|*#{name}|]]

callIfExists ::
  Member Rpc r =>
  MsgpackDecode a =>
  Text ->
  [Object] ->
  Sem r (Maybe a)
callIfExists name args =
  functionExists name >>= \case
    True -> Just <$> nvimCallFunction name args
    False -> pure Nothing
