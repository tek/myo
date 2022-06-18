module Myo.Network.Socket where

import Network.Socket (SockAddr (SockAddrUnix), Socket, socket)
import qualified Network.Socket as Socket (Family (AF_UNIX), SocketType (Datagram), bind)
import Path (Abs, File, Path, toFilePath)

unixSocket ::
  Member (Embed IO) r =>
  Sem r Socket
unixSocket =
  embed (socket Socket.AF_UNIX Socket.Datagram 0)

socketBind ::
  Member (Embed IO) r =>
  Path Abs File ->
  Sem r Socket
socketBind socketPath = do
  sock <- unixSocket
  sock <$ embed (Socket.bind sock $ SockAddrUnix (toFilePath socketPath))
