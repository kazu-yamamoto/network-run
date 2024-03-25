{-# LANGUAGE OverloadedStrings #-}

-- | Simple functions to run TCP clients and servers.
module Network.Run.TCP (
    runTCPClient,
    runTCPServer,

    -- * Generalized API
    runTCPServerWithSocket,
    openServerSocket,
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
    E.bracket (open addr) gclose client
  where
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

-- | Running a TCP server with an accepted socket and its peer name.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer = runTCPServerWithSocket openServerSocket

----------------------------------------------------------------
-- Generalized API

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
    addr <- resolve Stream mhost port True
    E.bracket (open addr) close loop
  where
    open addr = E.bracketOnError (initSocket addr) close $ \sock -> do
        listen sock 1024
        return sock
    loop sock = forever $
        E.bracketOnError (accept sock) (close . fst) $
            \(conn, _peer) ->
                void $ forkFinally (server conn) (const $ gclose conn)
