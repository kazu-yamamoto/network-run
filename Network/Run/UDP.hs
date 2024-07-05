-- | Simple functions to run UDP clients and servers.
module Network.Run.UDP (
    runUDPClient,
    runUDPServer,
    runUDPServerFork,
) where

import Control.Concurrent (forkFinally, forkIO)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Network.Socket
import Network.Socket.ByteString

import Network.Run.Core

-- | Running a UDP client with a socket.
--   The client action takes a socket and
--   server's socket address.
--   They should be used with 'sendTo'.
runUDPClient :: HostName -> ServiceName -> (Socket -> SockAddr -> IO a) -> IO a
runUDPClient host port client = withSocketsDo $ do
    addr <- resolve Datagram (Just host) port [AI_ADDRCONFIG]
    let sockAddr = addrAddress addr
    E.bracket (openSocket addr) close $ \sock -> client sock sockAddr

-- | Running a UDP server with an open socket in a single Haskell thread.
runUDPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runUDPServer mhost port server = withSocketsDo $ do
    addr <- resolve Datagram mhost port [AI_PASSIVE]
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
runUDPServerFork [] _ _ = return ()
runUDPServerFork (h : hs) port server = do
    mapM_ (forkIO . run) hs
    run h
  where
    run host = do
        labelMe $ "UDP server for " ++ h
        runUDPServer (Just host) port $ \lsock -> forever $ do
            (bs0, peeraddr) <- recvFrom lsock 2048
            let family = case peeraddr of
                    SockAddrInet{} -> AF_INET
                    SockAddrInet6{} -> AF_INET6
                    _ -> error "family"
                hints =
                    defaultHints
                        { addrSocketType = Datagram
                        , addrFamily = family
                        , addrFlags = [AI_PASSIVE]
                        }
            addr <- head <$> getAddrInfo (Just hints) Nothing (Just port)
            s <- openServerSocket addr
            connect s peeraddr
            void $ forkFinally (labelMe "UDP server" >> server s bs0) (\_ -> close s)
