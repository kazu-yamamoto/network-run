{-# LANGUAGE OverloadedStrings #-}

-- | Simple functions to run TCP clients and servers.
module Network.Run.TCP (
    runTCPClient,
    runTCPServer,

    -- * Generalized API
    runTCPServerWithSocket,
    runTCPServerWithSocketOptions,
    openServerSocket,
    openServerSocketWithOptions,
    runTCPClientWithSocket,
    runTCPClientWithSocketOptions,
    openClientSocket,
    openClientSocketWithOptions,
) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Network.Socket

import Network.Run.Core

-- | Running a TCP client with a connected socket.
--
-- This is the same as:
--
-- > runTCPClientWithSocketOptions []
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient = runTCPClientWithSocket openClientSocket

-- | Running a TCP client with a connected socket.
--
-- Sets the given socket options before connecting.
runTCPClientWithSocketOptions :: [(SocketOption, Int)] -> HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClientWithSocketOptions opts = runTCPClientWithSocket (openClientSocketWithOptions opts)

-- | Running a TCP server with an accepted socket and its peer name.
--
-- This is the same as:
--
-- > runTCPServerWithSocketOptions []
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer = runTCPServerWithSocket openServerSocket

-- | Running a TCP server with an accepted socket and its peer name.
--
-- Sets the given socket options before binding.
runTCPServerWithSocketOptions :: [(SocketOption, Int)] -> Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServerWithSocketOptions opts = runTCPServerWithSocket (openServerSocketWithOptions opts)

----------------------------------------------------------------
-- Generalized API

-- | Generalization of 'runTCPClient'
runTCPClientWithSocket
    :: (AddrInfo -> IO Socket)
    -- ^ Initialize socket.
    --
    -- This function is called while exceptions are masked.
    --
    -- The default (used by 'runTCPClient') is 'openClientSocket'.
    -> HostName
    -> ServiceName
    -> (Socket -> IO a)
    -> IO a
runTCPClientWithSocket initSocket host port client = withSocketsDo $ do
    addr <- resolve Stream (Just host) port [AI_ADDRCONFIG]
    E.bracket (open addr) close client
  where
    open addr = E.bracketOnError (initSocket addr) close return

-- | Generalization of 'runTCPServer'
runTCPServerWithSocket
    :: (AddrInfo -> IO Socket)
    -- ^ Initialize socket.
    --
    -- This function is called while exceptions are masked.
    --
    -- The default (used by 'runTCPServer') is 'openServerSocket'.
    -> Maybe HostName
    -> ServiceName
    -> (Socket -> IO a)
    -- ^ Called for each incoming connection, in a new thread
    -> IO a
runTCPServerWithSocket initSocket mhost port server = withSocketsDo $ do
    addr <- resolve Stream mhost port [AI_PASSIVE]
    E.bracket (open addr) close loop
  where
    open addr = E.bracketOnError (initSocket addr) close $ \sock -> do
        listen sock 1024
        return sock
    loop sock = forever $
        E.bracketOnError (accept sock) (close . fst) $
            \(conn, _peer) ->
                void $ forkFinally (labelMe "TCP server" >> server conn) (const $ gclose conn)
