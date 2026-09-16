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
    safeAccept,
) where

import Data.List.NonEmpty (NonEmpty)
import Control.Arrow
import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad (when)
import GHC.Conc.Sync
import Network.Socket
import Foreign.C.Error (Errno (..), eCONNABORTED)
import GHC.IO.Exception (IOErrorType (Interrupted), ioe_errno)
import System.IO.Error (ioeGetErrorType, isFullError)

resolve
    :: SocketType
    -> Maybe HostName
    -> ServiceName
    -> [AddrInfoFlag]
    -> (NonEmpty AddrInfo -> AddrInfo)
    -> IO AddrInfo
resolve socketType mhost port flags select =
    select <$> getAddrInfo (Just hints) mhost (Just port)
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

-- | Accepting a connection, retrying on transient errors.
--
-- 'accept' fails routinely for reasons which do not mean that the
-- listening socket is broken: the peer may reset the connection before
-- it is accepted (@ECONNABORTED@), or the process or the system may
-- have run out of file descriptors (@EMFILE@\/@ENFILE@).  Letting
-- these escape would terminate the accept loop, so they are retried
-- here.  Errors which do suggest a broken listening socket (@EBADF@,
-- @EINVAL@, ...) are re-thrown, which is also how a closed socket
-- stops the loop.
--
-- This function is interruptible: a blocked or sleeping retry still
-- receives asynchronous exceptions, so the server remains killable.
safeAccept :: Socket -> IO (Socket, SockAddr)
safeAccept sock = loop
  where
    loop = do
        ex <- E.try $ accept sock
        case ex of
            Right r -> return r
            Left e
                -- No descriptor is available at the moment.  Retrying
                -- at once would spin, since the listening socket stays
                -- readable.
                | isFullError e -> threadDelay emfileDelay >> loop
                -- These cost nothing; retry immediately.
                | ioeGetErrorType e == Interrupted -> loop
                | ioe_errno e == Just connAborted -> loop
                | otherwise -> E.throwIO e

    Errno connAborted = eCONNABORTED

emfileDelay :: Int
emfileDelay = 100000 -- 100 milliseconds
