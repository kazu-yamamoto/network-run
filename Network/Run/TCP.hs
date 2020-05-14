{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Simple functions to run TCP clients and servers.
module Network.Run.TCP (
    runTCPClient
  , runTCPServer
  ) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Network.Socket

import Network.Run.Core

-- | Running a TCP client with a connected socket.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve Stream (Just host) port False
#if MIN_VERSION_network(3,1,1)
    E.bracket (open addr) (\sock -> gracefulClose sock 5000) client
#else
    E.bracket (open addr) close client
#endif
  where
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

-- | Running a TCP server with an accepted socket and its peer name.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve Stream mhost port True
    E.bracket (open addr) close loop
  where
    open addr = E.bracketOnError (openServerSocket addr) close $ \sock -> do
        listen sock 1024
        return sock
    loop sock = forever $ E.bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) ->
#if MIN_VERSION_network(3,1,1)
          void $ forkFinally (server conn) (const $ gracefulClose conn 5000)
#else
          void $ forkFinally (server conn) (const $ close conn)
#endif
