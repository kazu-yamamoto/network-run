{-# LANGUAGE OverloadedStrings #-}

-- | Simple functions to run TCP clients and servers.
module Network.Run.TCP.Timeout (
    runTCPServer,
    TimeoutServer,

    -- * Generalized API
    runTCPServerWithSocket,
    openClientSocket,
    openServerSocket,
    runTCPServerWithListenSocket,
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
    addr <- resolve Stream mhost port [AI_PASSIVE]
    E.bracket (open addr) close $ \s ->
        runTCPServerWithListenSocket tm s server
  where
    open addr = E.bracketOnError (initSocket addr) close $ \sock -> do
        listen sock 1024
        return sock

-- | Another generalization of 'runTCPServer'
runTCPServerWithListenSocket
    :: Int
    -- ^ Timeout in second.
    -> Socket
    -- ^ A listen socket
    -> TimeoutServer a
    -- ^ Called for each incoming connection, in a new thread
    -> IO a
runTCPServerWithListenSocket tm sock server = withSocketsDo $ do
    T.withManager (tm * 1000000) loop
  where
    loop mgr = forever $
        E.bracketOnError (accept sock) (close . fst) $
            \(conn, _peer) ->
                void $ forkFinally (server' mgr conn) (const $ gclose conn)
    server' mgr conn = do
        th <- T.registerKillThread mgr $ return ()
        server mgr th conn
