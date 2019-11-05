-- | Simple functions to run UDP clients and servers.
module Network.Run.UDP (
    runUDPClient
  , runUDPServer
  , runUDPServerFork
  ) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Network.Socket
import Network.Socket.ByteString

import Network.Run.Core

-- | Running a UDP client with a socket.
--   The client action takes a socket and
--   server's socket address.
--   They should be used with 'sendTo'.
runUDPClient :: HostName -> ServiceName -> (Socket -> SockAddr -> IO a) -> IO a
runUDPClient host port client = withSocketsDo $ do
    addr <- resolve Datagram (Just host) port False
    let sockAddr = addrAddress addr
    E.bracket (openSocket addr) close $ \sock -> client sock sockAddr

-- | Running a UDP server with an open socket in a single Haskell thread.
runUDPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runUDPServer mhost port server = withSocketsDo $ do
    addr <- resolve Datagram mhost port True
    E.bracket (openServerSocket addr) close server

-- | Running a UDP server with a connected socket in each Haskell thread.
--   The first request is given to the server.
runUDPServerFork :: Maybe HostName -> ServiceName -> (Socket -> ByteString -> IO a) -> IO a
runUDPServerFork mhost port server = runUDPServer mhost port $ \lsock -> forever $ do
    (bs0,peeraddr) <- recvFrom lsock 1024
    let family = case peeraddr of
          SockAddrInet{}  -> AF_INET
          SockAddrInet6{} -> AF_INET6
          _                 -> error "family"
    s <- socket family Datagram defaultProtocol
    connect s peeraddr
    void $ forkFinally (server s bs0) (\_ -> close s)
