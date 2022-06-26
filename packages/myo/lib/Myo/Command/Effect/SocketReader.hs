module Myo.Command.Effect.SocketReader where

import Chiasma.Data.Ident (Ident)
import Conc (PScoped, pscoped)
import Path (Abs, File, Path)

data SocketReader :: Effect where
  Path :: SocketReader m (Path Abs File)
  Chunk :: SocketReader m (Maybe ByteString)

makeSem ''SocketReader

type ScopedSocketReader socket =
  PScoped Ident socket SocketReader

socketReader ::
  Member (ScopedSocketReader socket) r =>
  Ident ->
  InterpreterFor SocketReader r
socketReader =
  pscoped
