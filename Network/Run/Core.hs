{-# LANGUAGE CPP #-}

module Network.Run.Core (
    resolve,
    openSocket,
    openClientSocket,
    openClientSocketWithOptions,
    openClientSocketWithOpts,
    openServerSocket,
    openServerSocketWithOptions,
    openServerSocketWithOpts,
    openTCPServerSocket,
    openTCPServerSocketWithOptions,
    openTCPServerSocketWithOpts,
    gclose,
    labelMe,
) where

import Control.Arrow
import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad (when)
import Foreign (Storable)
import GHC.Conc.Sync
import Network.Socket

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
-- @
-- 'openClientSocketWithOptions' []
-- @
openClientSocket :: AddrInfo -> IO Socket
openClientSocket = openClientSocketWithOptions []

-- | Open a client socket with the given options
--
-- The options are set before 'connect'. This is equivalent to
--
-- @
-- 'openClientSocketWithOpts' . 'map' ('second' 'SockOptValue')
-- @
openClientSocketWithOptions :: [(SocketOption, Int)] -> AddrInfo -> IO Socket
openClientSocketWithOptions = openClientSocketWithOpts . map (second SockOptValue)

-- | Open a client socket with the given options
--
-- This must be used rather than 'openClientSocketWithOptions' for options such
-- as 'Network.Socket.Linger' which require a composite value
-- ('Network.Socket.StructLinger').
--
-- The options are set before 'connect'.
openClientSocketWithOpts :: [(SocketOption, SockOptValue)] -> AddrInfo -> IO Socket
openClientSocketWithOpts opts addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    mapM_ (uncurry $ setSockOptValue sock) opts
    connect sock $ addrAddress addr
    return sock

-- | Open socket for server use
--
-- This is the same as:
--
-- @
-- 'openServerSocketWithOptions' []
-- @
openServerSocket :: AddrInfo -> IO Socket
openServerSocket = openServerSocketWithOptions []

-- | Open socket for server use, and set the provided options before binding.
--
-- This is equivalent to
--
-- @
-- 'openServerSocketWithOpts' . 'map' ('second' 'SockOptValue')
-- @
openServerSocketWithOptions :: [(SocketOption, Int)] -> AddrInfo -> IO Socket
openServerSocketWithOptions = openServerSocketWithOpts . map (second SockOptValue)

-- | Open socket for server use, and set the provided options before binding.
--
-- In addition to the given options, the socket is configured to
--
-- * allow reuse of local addresses (SO_REUSEADDR)
-- * automatically be closed during a successful @execve@ (FD_CLOEXEC)
-- * bind to the address specified
openServerSocketWithOpts :: [(SocketOption, SockOptValue)] -> AddrInfo -> IO Socket
openServerSocketWithOpts opts addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
#if !defined(openbsd_HOST_OS)
    when (addrFamily addr == AF_INET6) $ setSocketOption sock IPv6Only 1
#endif
    mapM_ (uncurry $ setSockOptValue sock) opts
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock $ addrAddress addr
    return sock

-- | Open TCP socket for server use
--
-- This is the same as:
--
-- @
-- 'openTCPServerSocketWithOptions' []
-- @
openTCPServerSocket :: AddrInfo -> IO Socket
openTCPServerSocket = openTCPServerSocketWithOptions []

-- | Open socket for server use, and set the provided options before binding.
--
-- This is equivalent to
--
-- @
-- 'openTCPServerSocketWithOpts' . 'map' ('second' 'SockOptValue')
-- @
openTCPServerSocketWithOptions :: [(SocketOption, Int)] -> AddrInfo -> IO Socket
openTCPServerSocketWithOptions = openTCPServerSocketWithOpts . map (second SockOptValue)

-- | Open socket for server use, and set the provided options before binding.
--
-- In addition to the given options, the socket is configured to
--
-- * allow reuse of local addresses (SO_REUSEADDR)
-- * automatically be closed during a successful @execve@ (FD_CLOEXEC)
-- * bind to the address specified
-- * listen with queue length with 1024
openTCPServerSocketWithOpts :: [(SocketOption, SockOptValue)] -> AddrInfo -> IO Socket
openTCPServerSocketWithOpts opts addr = do
    sock <- openServerSocketWithOpts opts addr
    listen sock 1024
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
