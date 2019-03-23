module Myo.Network.Socket(
  unixSocket,
  socketBind,
) where

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.Socket (SockAddr(SockAddrUnix), Socket, socket)
import qualified Network.Socket as Socket (Family(AF_UNIX), SocketType(Datagram), bind)

unixSocket :: MonadBase IO m => m Socket
unixSocket =
  liftBase $ socket Socket.AF_UNIX Socket.Datagram 0

socketBind ::
  MonadBase IO m =>
  FilePath ->
  m Socket
socketBind filePath = do
  sock <- unixSocket
  liftBase $ Socket.bind sock $ SockAddrUnix filePath
  return sock
