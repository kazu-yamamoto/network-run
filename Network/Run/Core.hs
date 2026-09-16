{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

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
    labelMe,
    safeAccept,
    ServerSettings (..),
    defaultServerSettings,
    forkConnection,
    forkDatagram,
    report,
) where

import Control.Arrow hiding (loop)
import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad (void, when)
import Data.List.NonEmpty (NonEmpty)
import Foreign.C.Error (Errno (..), eCONNABORTED)
import GHC.Conc.Sync
import GHC.IO.Exception (IOErrorType (Interrupted), ioe_errno)
import Network.Socket
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
openClientSocketWithOpts
    :: [(SocketOption, SockOptValue)] -> AddrInfo -> IO Socket
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
-- * accept IPv6 only, rejecting IPv4-mapped addresses, if the address
--   family is 'AF_INET6' (IPV6_V6ONLY)
-- * automatically be closed during a successful @execve@ (FD_CLOEXEC)
-- * bind to the address specified
--
-- Because IPV6_V6ONLY is in effect, a socket bound to @::@ does not
-- accept IPv4 connections. To serve both families, open one socket per
-- address and run a server on each of them with
-- 'Network.Run.TCP.runTCPServerWithSocket'.
--
-- The given options are set after the ones above, so @(IPv6Only, 0)@
-- can be passed to ask for a dual stack socket. Note that OpenBSD
-- always makes IPv6 sockets IPv6 only; the option is not set there and
-- cannot be cleared.
openServerSocketWithOpts
    :: [(SocketOption, SockOptValue)] -> AddrInfo -> IO Socket
openServerSocketWithOpts opts addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
#if !defined(openbsd_HOST_OS)
    when (addrFamily addr == AF_INET6) $ setSocketOption sock IPv6Only 1
#endif
    mapM_ (uncurry $ setSockOptValue sock) opts
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock $ addrAddress addr
    return sock

-- | Open TCP socket for server use.
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
-- This is 'openServerSocketWithOpts' followed by 'listen' with a queue
-- length of 1024. See 'openServerSocketWithOpts' for the options which
-- are set in addition to the given ones.
--
-- This is equivalent to
--
-- @
-- 'openTCPServerSocketWithOpts' . 'map' ('second' 'SockOptValue')
-- @
openTCPServerSocketWithOptions :: [(SocketOption, Int)] -> AddrInfo -> IO Socket
openTCPServerSocketWithOptions = openTCPServerSocketWithOpts . map (second SockOptValue)

-- | Open socket for server use, and set the provided options before
-- binding.
--
-- This is 'openServerSocketWithOpts' followed by 'listen' with a queue
-- length of 1024.  See 'openServerSocketWithOpts' for the options which
-- are set in addition to the given ones.
openTCPServerSocketWithOpts
    :: [(SocketOption, SockOptValue)] -> AddrInfo -> IO Socket
openTCPServerSocketWithOpts opts addr = do
    sock <- openServerSocketWithOpts opts addr
    listen sock 1024
    return sock

labelMe :: String -> IO ()
labelMe name = do
    tid <- myThreadId
    labelThread tid name

----------------------------------------------------------------

-- | Settings for servers.
--
-- Fields which do not apply to a given server (for instance the
-- graceful close timeout for a UDP server) are ignored.
data ServerSettings = ServerSettings
    { settingsOnException :: Maybe SockAddr -> E.SomeException -> IO ()
    -- ^ Called when an exception is caught by the library instead of
    -- being propagated.  The 'SockAddr' is 'Just' the peer when the
    -- exception can be attributed to one.  Exceptions thrown by this
    -- action itself are discarded, so it must not be relied on for
    -- anything but reporting.  The default does nothing.
    , settingsGracefulCloseTimeout :: Int
    -- ^ Milliseconds 'gracefulClose' waits for the peer's FIN after a
    -- connection handler returns.  Zero or less uses 'close' instead,
    -- which releases the file descriptor immediately.  The default is
    -- 5000.
    , settingsAcceptRetryDelay :: Int
    -- ^ Microseconds to wait before retrying 'accept' after running
    -- out of file descriptors.  The default is 100000.
    }

-- | Default settings.  'settingsOnException' does nothing, so the
-- behaviour is the same as before this type was introduced.
defaultServerSettings :: ServerSettings
defaultServerSettings =
    ServerSettings
        { settingsOnException = \_ _ -> return ()
        , settingsGracefulCloseTimeout = 5000
        , settingsAcceptRetryDelay = 100000
        }

-- | Calling 'settingsOnException', never letting it throw.  A hook
-- must not be able to break a finalizer.
report :: ServerSettings -> Maybe SockAddr -> E.SomeException -> IO ()
report ServerSettings{..} mpeer se =
    settingsOnException mpeer se `E.catch` ignore
  where
    ignore :: E.SomeException -> IO ()
    ignore e
        | Just (E.SomeAsyncException _) <- E.fromException e = E.throwIO e
        | otherwise = return ()

-- | Closing a connected socket according to the settings.
gcloseWith :: ServerSettings -> Socket -> IO ()
gcloseWith ServerSettings{..} sock
    | settingsGracefulCloseTimeout <= 0 = close sock
    | otherwise = gracefulClose sock settingsGracefulCloseTimeout

----------------------------------------------------------------

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
-- Running out of file descriptors is passed to 'settingsOnException'
-- since a server which keeps hitting it is effectively out of service.
-- @ECONNABORTED@ and @EINTR@ are not, being routine.
--
-- This function is interruptible: a blocked or sleeping retry still
-- receives asynchronous exceptions, so the server remains killable.
safeAccept :: ServerSettings -> Socket -> IO (Socket, SockAddr)
safeAccept set@ServerSettings{..} sock = loop
  where
    loop = do
        ex <- E.try $ accept sock
        case ex of
            Right r -> return r
            Left e
                -- No descriptor is available at the moment.  Retrying
                -- at once would spin, since the listening socket stays
                -- readable.
                | isFullError e -> do
                    report set Nothing $ E.toException e
                    threadDelay settingsAcceptRetryDelay
                    loop
                -- These cost nothing; retry immediately.
                | ioeGetErrorType e == Interrupted -> loop
                | ioe_errno e == Just connAborted -> loop
                | otherwise -> E.throwIO e

    Errno connAborted = eCONNABORTED

----------------------------------------------------------------

-- | Forking a thread for an accepted socket.  An exception which
-- escapes the action is reported, and the socket is closed by the
-- given closer in either case.
forkWith
    :: ServerSettings
    -> (Socket -> IO ())
    -> Socket
    -> SockAddr
    -> IO a
    -> IO ()
forkWith set closer sock peer action = void $ forkFinally action finish
  where
    -- The socket must be closed even if the hook throws, which it does
    -- when an asynchronous exception arrives while it is running.
    finish er = reporting er `E.finally` closing

    reporting (Right _) = return ()
    reporting (Left se) = report set (Just peer) se

    closing = closer sock `E.catch` onCloseError

    onCloseError :: E.IOException -> IO ()
    onCloseError e = report set (Just peer) $ E.toException e

-- | 'forkWith' closing the socket gracefully.  For TCP.
forkConnection :: ServerSettings -> Socket -> SockAddr -> IO a -> IO ()
forkConnection set = forkWith set (gcloseWith set)

-- | 'forkWith' closing the socket immediately.  For UDP.
forkDatagram :: ServerSettings -> Socket -> SockAddr -> IO a -> IO ()
forkDatagram set = forkWith set close
