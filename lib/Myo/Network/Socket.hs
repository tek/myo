module Myo.Network.Socket where

import Control.Monad.Base (MonadBase, liftBase)
import Network.Socket (SockAddr(SockAddrUnix), Socket, socket)
import qualified Network.Socket as Socket (Family(AF_UNIX), SocketType(Datagram), bind)
import Path (Abs, File, Path, toFilePath)

unixSocket :: MonadBase IO m => m Socket
unixSocket =
  liftBase $ socket Socket.AF_UNIX Socket.Datagram 0

socketBind ::
  MonadBase IO m =>
  Path Abs File ->
  m Socket
socketBind socketPath = do
  sock <- unixSocket
  liftBase $ Socket.bind sock $ SockAddrUnix (toFilePath socketPath)
  return sock
