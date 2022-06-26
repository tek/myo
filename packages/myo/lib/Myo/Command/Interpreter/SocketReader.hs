module Myo.Command.Interpreter.SocketReader where

import Chiasma.Data.Ident (Ident)
import Conc (interpretPScopedResumable)
import Data.ByteString (hGetSome)
import qualified Network.Socket as Socket
import Network.Socket (socketToHandle)
import Path (Abs, File, Path, parent, toFilePath)
import Path.IO (createDirIfMissing)
import System.IO (Handle, IOMode (ReadMode), hClose)

import Myo.Command.Data.LogDir (LogDir)
import Myo.Command.Data.SocketReaderError (SocketReaderError (BindFailed, InvalidIdent))
import Myo.Command.Effect.SocketReader (ScopedSocketReader, SocketReader (Chunk, Path))
import Myo.Command.Log (logPath)

data SocketReaderResources =
  SocketReaderResources Handle (Path Abs File)
  deriving stock (Eq, Show)

withSocket ::
  Members [Reader LogDir, Resource, Embed IO] r =>
  Ident ->
  (SocketReaderResources -> Sem (Stop SocketReaderError : r) a) ->
  Sem (Stop SocketReaderError : r) a
withSocket ident use =
  bracket acquire release \ (_, handle, path) -> use (SocketReaderResources handle path)
  where
    acquire = do
      path <- stopNote (InvalidIdent ident) =<< logPath ident
      stopTryIOError BindFailed do
        createDirIfMissing True (parent path)
        socket <- Socket.socket Socket.AF_UNIX Socket.Datagram 0
        Socket.bind socket (Socket.SockAddrUnix (toFilePath path))
        handle <- socketToHandle socket ReadMode
        pure (socket, handle, path)
    release (socket, handle, _) = do
      tryIOError_ (hClose handle)
      tryIOError_ (Socket.close socket)

interpretSocketReader ::
  Members [Reader LogDir, Resource, Embed IO] r =>
  InterpreterFor (ScopedSocketReader SocketReaderResources !! SocketReaderError) r
interpretSocketReader =
  interpretPScopedResumable withSocket \ (SocketReaderResources handle path) -> \case
    Path ->
      pure path
    Chunk ->
      tryIOErrorMaybe (hGetSome handle 32000)
