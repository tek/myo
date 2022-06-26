module Myo.Network.Socket where

import qualified Network.Socket as Socket
import Network.Socket (SockAddr (SockAddrUnix), Socket, socket)
import Path (Abs, File, Path, toFilePath)

unixSocket ::
  Member (Embed IO) r =>
  Sem r Socket
unixSocket =
  embed (socket Socket.AF_UNIX Socket.Stream 0)

socketBind ::
  Member (Embed IO) r =>
  Path Abs File ->
  Sem r Socket
socketBind socketPath = do
  sock <- unixSocket
  sock <$ embed (Socket.bind sock (SockAddrUnix (toFilePath socketPath)))
