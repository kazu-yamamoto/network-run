-- | Simple functions to run UDP clients and servers.
module Network.Run.UDP (
    runUDPClient,
    runUDPServer,
    runUDPServerFork,
    runUDPServerForkWithSettings,
    ServerSettings (..),
    defaultServerSettings,
) where

import Control.Concurrent (forkIO)
import qualified Control.Exception as E
import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NE
import Network.Socket
import Network.Socket.ByteString

import Network.Run.Core

-- | Running a UDP client with a socket.
--   The client action takes a socket and
--   server's socket address.
--   They should be used with 'sendTo'.
runUDPClient :: HostName -> ServiceName -> (Socket -> SockAddr -> IO a) -> IO a
runUDPClient host port client = do
    addr <- resolve Datagram (Just host) port [AI_ADDRCONFIG] NE.head
    let sockAddr = addrAddress addr
    E.bracket (openSocket addr) close $ \sock -> client sock sockAddr

-- | Running a UDP server with an open socket in a single Haskell thread.
runUDPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runUDPServer mhost port server = do
    addr <- resolve Datagram mhost port [AI_PASSIVE] NE.head
    E.bracket (openServerSocket addr) close server

-- | Running a UDP server with a connected socket in each Haskell thread.
--   The first request is given to the server.
--   Suppose that the server is serving on __addrS:portS__ and
--   a client connects to the service from __addrC:portC__.
--   A connected socket is created by binding to __*:portS__ and
--   connecting to __addrC:portC__,
--   resulting in __(UDP,addrS:portS,addrC:portC)__ where
--   __addrS__ is given magically.
--   This approach is fragile due to NAT rebidings.
runUDPServerFork
    :: [HostName] -> ServiceName -> (Socket -> ByteString -> IO ()) -> IO ()
runUDPServerFork = runUDPServerForkWithSettings defaultServerSettings

-- | 'runUDPServerFork' with the given settings.
runUDPServerForkWithSettings
    :: ServerSettings
    -> [HostName]
    -> ServiceName
    -> (Socket -> ByteString -> IO ())
    -> IO ()
runUDPServerForkWithSettings _ [] _ _ = return ()
runUDPServerForkWithSettings set (h : hs) port server = do
    mapM_ (forkIO . run) hs
    run h
  where
    run host = do
        labelMe $ "UDP server for " ++ host
        runUDPServer (Just host) port $ \lsock -> forever $ do
            -- An error from 'recvFrom' means that the listening socket
            -- itself is gone, so it is left to propagate as before.
            (bs0, peeraddr) <- recvFrom lsock 2048
            -- Everything below is per-datagram work.  A failure here
            -- must not take the entire server down.
            dispatch peeraddr bs0 `E.catch` onDispatchError peeraddr

    onDispatchError peeraddr e =
        report set (Just peeraddr) $ E.toException (e :: E.IOException)

    dispatch peeraddr bs0 = case familyOf peeraddr of
        -- Neither IPv4 nor IPv6.  Just drop the datagram.
        Nothing -> return ()
        Just family -> do
            let hints =
                    defaultHints
                        { addrSocketType = Datagram
                        , addrFamily = family
                        , addrFlags = [AI_PASSIVE]
                        }
            addr <- NE.head <$> getAddrInfo (Just hints) Nothing (Just port)
            -- If 'connect' throws, the socket is closed here.  On
            -- success it is owned by the new thread and is closed by
            -- its finalizer.
            E.bracketOnError (openServerSocket addr) close $ \s -> do
                connect s peeraddr
                forkDatagram set s peeraddr $
                    labelMe "UDP server" >> server s bs0

    familyOf SockAddrInet{} = Just AF_INET
    familyOf SockAddrInet6{} = Just AF_INET6
    familyOf _ = Nothing
