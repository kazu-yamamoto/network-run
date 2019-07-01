{-# LANGUAGE OverloadedStrings #-}

-- | Simple functions to run TCP clients and servers.
module Network.Run.TCP (
    runTCPClient
  , runTCPServer
  ) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void, when)
import Network.Socket
import qualified Network.Socket.ByteString as NSB

-- | Running a TCP client with a connected socket.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) gracefulClose client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock

-- | Running a TCP server with an accepted socket and its peer name.
runTCPServer :: String -> (Socket -> SockAddr -> IO a) -> IO a
runTCPServer port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) Nothing (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock $ setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = forever $ do
        (conn, peer) <- accept sock
        void $ forkFinally (server conn peer) (const $ gracefulClose conn)

gracefulClose :: Socket -> IO ()
gracefulClose conn = (sendRecvFIN `E.finally` close conn) `E.catch` ignore
  where
    sendRecvFIN = do
        -- Sending TCP FIN.
        shutdown conn ShutdownSend
        -- Waiting TCP FIN.
        recvFIN
    recvFIN = do
        -- Don't use 4092 here.
        -- The GHC runtime takes the global lock
        -- if the length of ByteString is over 3276 bytes in 32bit
        -- or 3272 bytes in 64bit.
        bs <- NSB.recv conn 1024
        when (bs /= "") recvFIN
    -- shutdown sometime returns ENOTCONN.
    -- Probably, we don't want to log this error.
    ignore (E.SomeException _) = return ()
