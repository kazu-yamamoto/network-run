-- | Simple functions to run UDP clients and servers.
module Network.Run.UDP (
    runUDPClient
  , runUDPServer
  ) where

import qualified Control.Exception as E
import Network.Socket

import Network.Run.Core

-- | Running a UDP client with a socket.
--   The client action takes a socket and
--   server's socket address.
--   They should be used with 'sendTo'.
runUDPClient :: String -> String -> (Socket -> SockAddr -> IO a) -> IO a
runUDPClient = runClient Datagram

-- | Running a UDP server with an open socket.
runUDPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runUDPServer mhost port server = withSocketsDo $ do
    addr <- resolve Datagram mhost port True
    E.bracket (openServerSocket addr) close server
