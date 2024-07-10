{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Run.Core (
    resolve,
    openSocket,
    openClientSocket,
    openClientSocketWithOptions,
    openServerSocket,
    openServerSocketWithOptions,
    gclose,
    labelMe,
) where

import qualified Control.Exception as E
import Control.Monad (when)
import Network.Socket
import GHC.Conc.Sync

resolve
    :: SocketType
    -> Maybe HostName
    -> ServiceName
    -> [AddrInfoFlag]
    -> IO AddrInfo
resolve socketType mhost port flags =
    head <$> getAddrInfo (Just hints) mhost (Just port)
  where
    hints =
        defaultHints
            { addrSocketType = socketType
            , addrFlags = flags
            }

#if !MIN_VERSION_network(3,1,2)
openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
#endif

-- | This is the same as
--
-- > openClientSocketWithOptions []
openClientSocket :: AddrInfo -> IO Socket
openClientSocket = openClientSocketWithOptions []

openClientSocketWithOptions :: [(SocketOption, Int)] -> AddrInfo -> IO Socket
openClientSocketWithOptions opts addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    mapM_ (uncurry $ setSocketOption sock) opts
    connect sock $ addrAddress addr
    return sock

-- | Open socket for server use
--
-- This is the same as:
--
-- > openServerSocketWithOptions []
openServerSocket :: AddrInfo -> IO Socket
openServerSocket = openServerSocketWithOptions []

-- | Open socket for server use, and set the provided options before binding.
--
-- In addition to the given options, the socket is configured to
--
-- * allow reuse of local addresses (SO_REUSEADDR)
-- * automatically be closed during a successful @execve@ (FD_CLOEXEC)
-- * bind to the address specified
--
-- Don't forget to call 'listen' for TCP after this.
openServerSocketWithOptions :: [(SocketOption, Int)] -> AddrInfo -> IO Socket
openServerSocketWithOptions opts addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
#if !defined(openbsd_HOST_OS)
    when (addrFamily addr == AF_INET6) $ setSocketOption sock IPv6Only 1
#endif
    mapM_ (uncurry $ setSocketOption sock) opts
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock $ addrAddress addr
    return sock

gclose :: Socket -> IO ()
#if MIN_VERSION_network(3,1,1)
gclose sock = gracefulClose sock 5000
#else
gclose = close
#endif

labelMe :: String -> IO ()
labelMe name = do
    tid <- myThreadId
    labelThread tid name
