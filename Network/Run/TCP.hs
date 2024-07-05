{-# LANGUAGE OverloadedStrings #-}

-- | Simple functions to run TCP clients and servers.
module Network.Run.TCP (
    -- * Server
    runTCPServer,
    runTCPServerWithSocket,
    openServerSocket,
    openServerSocketWithOptions,

    -- * Client
    runTCPClient,
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

----------------------------------------------------------------

-- | Running a TCP server with an accepted socket and its peer name.
--
-- This is the same as:
--
-- > runTCPServerWithSocketOptions []
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve Stream mhost port [AI_PASSIVE]
    E.bracket (open addr) close $ \sock ->
        runTCPServerWithSocket sock server
  where
    open addr = do
        sock <- openServerSocket addr
        listen sock 1024
        return sock

-- | Running a TCP client with a connected socket for a given listen
-- socket.
runTCPServerWithSocket
    :: Socket
    -> (Socket -> IO a)
    -- ^ Called for each incoming connection, in a new thread
    -> IO a
runTCPServerWithSocket sock server = withSocketsDo $
    forever $
        E.bracketOnError (accept sock) (close . fst) $
            \(conn, _peer) ->
                void $ forkFinally (labelMe "TCP server" >> server conn) (const $ gclose conn)

----------------------------------------------------------------

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
runTCPClientWithSocketOptions
    :: [(SocketOption, Int)] -> HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClientWithSocketOptions opts = runTCPClientWithSocket (openClientSocketWithOptions opts)

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
    E.bracket (initSocket addr) close client
