module Myo.Command.Effect.SocketReader where

import Conc (PScoped, pscoped)
import Path (Abs, File, Path)

import Myo.Data.CommandId (CommandId)

data SocketReader :: Effect where
  Path :: SocketReader m (Path Abs File)
  Chunk :: SocketReader m (Maybe ByteString)

makeSem ''SocketReader

type ScopedSocketReader socket =
  PScoped CommandId socket SocketReader

socketReader ::
  Member (ScopedSocketReader socket) r =>
  CommandId ->
  InterpreterFor SocketReader r
socketReader =
  pscoped
