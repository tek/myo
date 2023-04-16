module Myo.Command.Effect.SocketReader where

import Path (Abs, File, Path)

import Myo.Data.CommandId (CommandId)

data SocketReader :: Effect where
  Path :: SocketReader m (Path Abs File)
  Chunk :: SocketReader m (Maybe ByteString)

makeSem ''SocketReader

type ScopedSocketReader =
  Scoped CommandId SocketReader

socketReader ::
  Member ScopedSocketReader r =>
  CommandId ->
  InterpreterFor SocketReader r
socketReader =
  scoped
