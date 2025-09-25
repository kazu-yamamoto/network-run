{-# LANGUAGE OverloadedStrings #-}

-- | Simple functions to run TCP clients and servers.
module Network.Run.TCP.Timeout (
    runTCPServer,
    TimeoutServer,

    -- * Generalized API
    runTCPServerWithSocket,
    openServerSocket,
    openServerSocketWithOptions,
    openServerSocketWithOpts,
) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import qualified Data.List.NonEmpty as NE
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

-- | Running a TCP server with a connected socket.
runTCPServer
    :: Int
    -- ^ Timeout in second.
    -> Maybe HostName
    -> ServiceName
    -> TimeoutServer ()
    -> IO ()
runTCPServer tm mhost port server = do
    addr <- resolve Stream mhost port [AI_PASSIVE] NE.head
    E.bracket (openTCPServerSocket addr) close $ \sock ->
        runTCPServerWithSocket tm sock server

-- | Running a TCP client with a connected socket for a given listen
-- socket.
runTCPServerWithSocket
    :: Int
    -- ^ Timeout in second.
    -> Socket
    -> TimeoutServer ()
    -> IO ()
runTCPServerWithSocket tm sock server = do
    T.withManager (tm * 1000000) $ \mgr -> forever $
        E.bracketOnError (accept sock) (close . fst) $ \(conn, _peer) ->
            void $ forkFinally (runServer mgr conn) (const $ gclose conn)
  where
    runServer mgr conn = do
        labelMe "TCP timeout server"
        T.withHandleKillThread mgr (return ()) $ \th -> server mgr th conn
