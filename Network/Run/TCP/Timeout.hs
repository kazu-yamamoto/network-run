-- | Simple functions to run TCP servers.
module Network.Run.TCP.Timeout (
    runTCPServer,
    runTCPServerWithSettings,
    TimeoutServer,
    ServerSettings (..),
    defaultServerSettings,
    resolve,

    -- * Generalized API
    runTCPServerWithSocket,
    runTCPServerWithSocketAndSettings,
    openTCPServerSocket,
    openTCPServerSocketWithOptions,
    openTCPServerSocketWithOpts,
) where

import qualified Control.Exception as E
import Control.Monad (forever)
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

-- | Running a TCP server, resolving and binding the address itself.
--
-- Only the first address returned for @mhost@ is used, so a server
-- created by this function listens on a single address family.  Use
-- 'runTCPServerWithSocket' with one socket per address to serve both
-- IPv4 and IPv6.
runTCPServer
    :: Int
    -- ^ Timeout in second.
    -> Maybe HostName
    -> ServiceName
    -> TimeoutServer ()
    -> IO ()
runTCPServer = runTCPServerWithSettings defaultServerSettings

-- | Running a TCP server with the given settings.
runTCPServerWithSettings
    :: ServerSettings
    -> Int
    -- ^ Timeout in second.
    -> Maybe HostName
    -> ServiceName
    -> TimeoutServer ()
    -> IO ()
runTCPServerWithSettings set tm mhost port server = do
    addr <- resolve Stream mhost port [AI_PASSIVE] NE.head
    E.bracket (openTCPServerSocket addr) close $ \sock ->
        runTCPServerWithSocketAndSettings set tm sock server

-- | Running a TCP server on a given listen socket.
runTCPServerWithSocket
    :: Int
    -- ^ Timeout in second.
    -> Socket
    -- ^ A listening socket created by 'openTCPServerSocket'.
    -> TimeoutServer ()
    -> IO ()
runTCPServerWithSocket = runTCPServerWithSocketAndSettings defaultServerSettings

-- | Running a TCP server on a given listen socket with the given
-- settings.
runTCPServerWithSocketAndSettings
    :: ServerSettings
    -> Int
    -- ^ Timeout in second.
    -> Socket
    -- ^ A listening socket created by 'openTCPServerSocket'.
    -> TimeoutServer ()
    -> IO ()
runTCPServerWithSocketAndSettings set tm sock server =
    T.withManager (tm * 1000000) $ \mgr -> forever $
        E.bracketOnError (safeAccept set sock) (close . fst) $ \(conn, peer) ->
            forkConnection set conn peer (runServer mgr conn)
  where
    runServer mgr conn = do
        labelMe "TCP timeout server"
        T.withHandleKillThread mgr (return ()) $ \th -> server mgr th conn
