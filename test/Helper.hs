{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Helpers shared by the specs.
--
-- Two rules keep these tests reliable:
--
-- * A server is always given port @0@ and its real port is read back
--   with 'getSocketName', so nothing depends on a fixed port and the
--   suite can be run concurrently with anything else.
--
-- * Every test body is wrapped in 'limited'.  The library is full of
--   'forever' loops, so a regression must fail the suite instead of
--   hanging it.
module Helper where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.List.NonEmpty as NE
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket
import Network.Socket.ByteString
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Timeout (timeout)

import Network.Run.Core
import Network.Run.TCP (runTCPClient, runTCPServerWithSocketAndSettings)

----------------------------------------------------------------

-- | Failing instead of hanging.
limited :: IO a -> IO a
limited action = do
    ma <- timeout (10 * 1000000) action
    case ma of
        Nothing -> E.throwIO $ userError "the test did not finish in time"
        Just a -> return a

loopback :: HostName
loopback = "127.0.0.1"

portOf :: SockAddr -> PortNumber
portOf (SockAddrInet p _) = p
portOf (SockAddrInet6 p _ _ _) = p
portOf sa = error $ "portOf: " ++ show sa

-- | Elapsed milliseconds of an action.
elapsed :: IO a -> IO (a, Int)
elapsed action = do
    t0 <- getMonotonicTimeNSec
    a <- action
    t1 <- getMonotonicTimeNSec
    return (a, fromIntegral ((t1 - t0) `div` 1000000))

ignoreAny :: IO a -> IO ()
ignoreAny action = void action `E.catch` \(_ :: E.SomeException) -> return ()

----------------------------------------------------------------

-- | A TCP listening socket on an ephemeral port of the loopback.
withListenSocket :: (Socket -> PortNumber -> IO a) -> IO a
withListenSocket body = do
    addr <- resolve Stream (Just loopback) "0" [AI_PASSIVE] NE.head
    E.bracket (openTCPServerSocket addr) close $ \lsock -> do
        port <- portOf <$> getSocketName lsock
        body lsock port

-- | Running a TCP server on an ephemeral port while the body runs.
withTCPServer
    :: ServerSettings -> (Socket -> IO ()) -> (PortNumber -> IO a) -> IO a
withTCPServer set server body = withListenSocket $ \lsock port ->
    E.bracket
        (forkIO $ runTCPServerWithSocketAndSettings set lsock server)
        killThread
        (\_ -> body port)

client :: PortNumber -> (Socket -> IO a) -> IO a
client port = runTCPClient loopback (show port)

-- | One request and one response on a fresh connection.
request :: PortNumber -> ByteString -> IO ByteString
request port bs = client port $ \sock -> sendAll sock bs >> recv sock 1024

echo :: Socket -> IO ()
echo sock = loop
  where
    loop = do
        bs <- recv sock 1024
        unless (BS.null bs) $ sendAll sock bs >> loop

----------------------------------------------------------------

-- | What 'settingsOnException' was called with.
type Report = (Maybe SockAddr, String)

-- | Settings which record every reported exception.
collecting :: IO (ServerSettings, IO [Report])
collecting = do
    ref <- newIORef []
    let set =
            defaultServerSettings
                { settingsOnException = \mpeer se ->
                    atomicModifyIORef' ref $ \rs -> (rs ++ [(mpeer, show se)], ())
                }
    return (set, readIORef ref)

-- | Waiting until at least @n@ items are available, since reporting
-- happens in another thread.
waitFor :: Int -> IO [a] -> IO [a]
waitFor n getter = go (300 :: Int)
  where
    go 0 = getter
    go k = do
        xs <- getter
        if length xs >= n
            then return xs
            else threadDelay 10000 >> go (k - 1)

----------------------------------------------------------------

-- | Whether this machine can open an IPv6 socket at all.
hasIPv6 :: IO Bool
hasIPv6 = do
    er <- E.try go
    return $ either (const False) (const True) (er :: Either E.IOException ())
  where
    go = do
        addr <- resolve Stream (Just "::1") "0" [AI_PASSIVE] NE.head
        E.bracket (openServerSocket addr) close $ \_ -> return ()

-- | The number of open file descriptors, where the platform shows them.
openFds :: IO (Maybe Int)
openFds = go ["/proc/self/fd", "/dev/fd"]
  where
    go [] = return Nothing
    go (d : ds) = do
        exist <- doesDirectoryExist d
        if exist
            then Just . length <$> getDirectoryContents d
            else go ds
