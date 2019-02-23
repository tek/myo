module Myo.Network.Socket(
  unixSocket,
  socketBind,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.Socket (Socket, SockAddr(SockAddrUnix), socket)
import qualified Network.Socket as Socket (Family(AF_UNIX), SocketType(Datagram), bind)

unixSocket :: (MonadIO m) => m Socket
unixSocket =
  liftIO $ socket Socket.AF_UNIX Socket.Datagram 0

socketBind ::
  (MonadIO m) =>
  FilePath ->
  m Socket
socketBind filePath = do
  sock <- unixSocket
  liftIO $ Socket.bind sock $ SockAddrUnix filePath
  return sock
