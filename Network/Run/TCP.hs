{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Simple functions to run TCP clients and servers.
module Network.Run.TCP (
    -- * Server
    runTCPServer,
    runTCPServerWithSocket,
    openTCPServerSocket,
    openTCPServerSocketWithOptions,
    openTCPServerSocketWithOpts,
    resolve,

    -- * Client
    runTCPClient,
    Settings,
    defaultSettings,
    settingsOpenClientSocket,
    settingsSelectAddrInfo,
    runTCPClientWithSettings,
    openClientSocket,
    openClientSocketWithOptions,
    openClientSocketWithOpts,
) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Network.Socket

import Network.Run.Core

----------------------------------------------------------------

-- | Running a TCP server with an accepted socket and its peer name.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = do
    addr <- resolve Stream mhost port [AI_PASSIVE] NE.head
    E.bracket (openTCPServerSocket addr) close $ \sock ->
        runTCPServerWithSocket sock server

-- | Running a TCP client with a connected socket for a given listen
-- socket.
runTCPServerWithSocket
    :: Socket
    -> (Socket -> IO a)
    -- ^ Called for each incoming connection, in a new thread
    -> IO a
runTCPServerWithSocket sock server = forever $
    E.bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) ->
            void $ forkFinally (labelMe "TCP server" >> server conn) (const $ gclose conn)

----------------------------------------------------------------

-- | Settings for client.
data Settings = Settings
    { settingsOpenClientSocket :: AddrInfo -> IO Socket
    -- ^ Opening a socket. Use 'openClientSocketWithOptions' to specify 'SocketOption'
    , settingsSelectAddrInfo :: NonEmpty AddrInfo -> AddrInfo
    -- ^ Selecting 'AddrInfo'.
    }

-- | Default settings.
defaultSettings :: Settings
defaultSettings =
    Settings
        { settingsOpenClientSocket = openClientSocket
        , settingsSelectAddrInfo = NE.head
        }

-- | Running a TCP client with a connected socket.
--
-- This is the same as:
--
-- @
-- 'runTCPClientWithSettings' 'defaultSettings'
-- @
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient = runTCPClientWithSettings defaultSettings

-- | Running a TCP client with a connected socket.
runTCPClientWithSettings
    :: Settings
    -> HostName
    -> ServiceName
    -> (Socket -> IO a)
    -> IO a
runTCPClientWithSettings Settings{..} host port client = do
    addr <- resolve Stream (Just host) port [AI_ADDRCONFIG] settingsSelectAddrInfo
    E.bracket (settingsOpenClientSocket addr) close client
