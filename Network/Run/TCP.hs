{-# LANGUAGE RecordWildCards #-}

-- | Simple functions to run TCP clients and servers.
module Network.Run.TCP (
    -- * Server
    runTCPServer,
    runTCPServerWithSettings,
    runTCPServerWithSocket,
    runTCPServerWithSocketAndSettings,
    ServerSettings (..),
    defaultServerSettings,
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

import qualified Control.Exception as E
import Control.Monad (forever)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Network.Socket

import Network.Run.Core

----------------------------------------------------------------

-- | Running a TCP server with an accepted socket.
--
-- Only the first address returned for @mhost@ is used, so a server
-- created by this function listens on a single address family. Use
-- 'runTCPServerWithSocket' with one socket per address to serve both
-- IPv4 and IPv6.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer = runTCPServerWithSettings defaultServerSettings

-- | Running a TCP server with the given settings.
runTCPServerWithSettings
    :: ServerSettings
    -> Maybe HostName
    -> ServiceName
    -> (Socket -> IO a)
    -> IO a
runTCPServerWithSettings set mhost port server = do
    addr <- resolve Stream mhost port [AI_PASSIVE] NE.head
    E.bracket (openTCPServerSocket addr) close $ \sock ->
        runTCPServerWithSocketAndSettings set sock server

-- | Running a TCP server on a given listen socket.
runTCPServerWithSocket
    :: Socket
    -- ^ A listening socket created by 'openTCPServerSocket'.
    -> (Socket -> IO a)
    -- ^ Called for each incoming connection, in a new thread
    -> IO a
runTCPServerWithSocket = runTCPServerWithSocketAndSettings defaultServerSettings

-- | Running a TCP server on a given listen socket with the given
-- settings.
runTCPServerWithSocketAndSettings
    :: ServerSettings
    -> Socket
    -- ^ A listening socket created by 'openTCPServerSocket'.
    -> (Socket -> IO a)
    -- ^ Called for each incoming connection, in a new thread
    -> IO a
runTCPServerWithSocketAndSettings set sock server = forever $
    E.bracketOnError (safeAccept set sock) (close . fst) $ \(conn, peer) ->
        forkConnection set conn peer (labelMe "TCP server" >> server conn)

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
