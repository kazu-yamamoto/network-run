{-# LANGUAGE OverloadedStrings #-}

-- | Simple functions to run TCP clients and servers.
module Network.Run.TCP.Timeout (
    runTCPServer,
    TimeoutServer,

    -- * Generalized API
    runTCPServerWithSocket,
    runTCPServerWithSocketOptions,
    openServerSocket,
    openServerSocketWithOptions,
) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Network.Socket
import qualified System.TimeManager as T

import Network.Run.Core

-- | A server type
type TimeoutServer a =
    T.Manager
    -- ^ A global timeout manager
    -> T.Handle
    -- ^ A thread-local timeout handler
    -> Socket
    -- ^ A connected socket
    -> IO a

-- | Running a TCP server with an accepted socket and its peer name.
runTCPServer
    :: Int
    -- ^ Timeout in second.
    -> Maybe HostName
    -> ServiceName
    -> TimeoutServer a
    -> IO a
runTCPServer = runTCPServerWithSocket openServerSocket

-- | Running a TCP server with an accepted socket and its peer name.
--
-- Sets the given socket options on the socket before binding.
runTCPServerWithSocketOptions
    :: [(SocketOption, Int)]
    -> Int
    -- ^ Timeout in second.
    -> Maybe HostName
    -> ServiceName
    -> TimeoutServer a
    -> IO a
runTCPServerWithSocketOptions opts = runTCPServerWithSocket (openServerSocketWithOptions opts)

----------------------------------------------------------------
-- Generalized API

-- | Generalization of 'runTCPServer'
--
-- See 'Network.Run.TCP.runTCPServerWithSocket' for additional discussion.
runTCPServerWithSocket
    :: (AddrInfo -> IO Socket)
    -> Int
    -- ^ Timeout in second.
    -> Maybe HostName
    -> ServiceName
    -> TimeoutServer a
    -> IO a
runTCPServerWithSocket initSocket tm mhost port server = withSocketsDo $ do
    T.withManager (tm * 1000000) $ \mgr -> do
        addr <- resolve Stream mhost port [AI_PASSIVE]
        E.bracket (open addr) close $ loop mgr
  where
    open addr = E.bracketOnError (initSocket addr) close $ \sock -> do
        listen sock 1024
        return sock
    loop mgr sock = forever $
        E.bracketOnError (accept sock) (close . fst) $
            \(conn, _peer) ->
                void $ forkFinally (server' mgr conn) (const $ gclose conn)
    server' mgr conn = do
        th <- T.registerKillThread mgr $ return ()
        server mgr th conn
