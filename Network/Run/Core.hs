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
    safeAcceptWith,
    Listener (..),
    makeListener,
    ServerSettings (..),
    defaultServerSettings,
    gcloseWith,
    forkWith,
    forkConnection,
    forkDatagram,
    report,
) where

import Control.Applicative ((<|>))
import Control.Arrow hiding (loop)
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (void, when)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import Foreign.C.Error (Errno (..), eCONNABORTED)
import Foreign.C.Types (CInt)
import GHC.Conc.Sync
import GHC.IO.Exception (
    IOErrorType (Interrupted, InvalidArgument, UnsupportedOperation),
    ioe_errno,
 )
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
-- 'openClientSocketWithOpts' . 'map' ('second' ('SockOptValue' . 'fromIntegral' \@Int \@CInt))
-- @
openClientSocketWithOptions :: [(SocketOption, Int)] -> AddrInfo -> IO Socket
openClientSocketWithOptions = openClientSocketWithOpts . map (second sockOptInt)

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
-- 'openServerSocketWithOpts' . 'map' ('second' ('SockOptValue' . 'fromIntegral' \@Int \@CInt))
-- @
openServerSocketWithOptions :: [(SocketOption, Int)] -> AddrInfo -> IO Socket
openServerSocketWithOptions = openServerSocketWithOpts . map (second sockOptInt)

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
-- 'openTCPServerSocketWithOpts' . 'map' ('second' ('SockOptValue' . 'fromIntegral' \@Int \@CInt))
-- @
openTCPServerSocketWithOptions :: [(SocketOption, Int)] -> AddrInfo -> IO Socket
openTCPServerSocketWithOptions = openTCPServerSocketWithOpts . map (second sockOptInt)

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

-- | An 'Int' option as a 'SockOptValue'.
--
-- The value must be converted to 'CInt' first.  A socket option is an
-- @int@ in C, and 'SockOptValue' passes @sizeof@ of the value it is
-- given, so an 'Int' asks the kernel to read eight bytes.  Linux
-- ignores the extra ones, but BSD rejects the call with @EINVAL@.
sockOptInt :: Int -> SockOptValue
sockOptInt = SockOptValue . (fromIntegral :: Int -> CInt)

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
    , settingsInstallShutdownHandler :: IO () -> IO ()
    -- ^ An action to install a handler (a Unix signal handler, say) which
    -- stops the server.  The first argument is the action which does that:
    -- it stops the loop accepting, and the listening socket is closed
    -- afterwards by whoever opened it.  It is not an action which closes
    -- that socket, and should not be used as one.  The default does
    -- nothing, and the server is then stopped the other way, by an
    -- asynchronous exception to the thread running it.
    }

-- | Default settings.  'settingsOnException' does nothing, so the
-- behaviour is the same as before this type was introduced.
defaultServerSettings :: ServerSettings
defaultServerSettings =
    ServerSettings
        { settingsOnException = \_ _ -> return ()
        , settingsGracefulCloseTimeout = 5000
        , settingsAcceptRetryDelay = 100000
        , settingsInstallShutdownHandler = const $ return ()
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

-- | What an accept loop waits on.
data Listener = Listener
    { waitAcceptable :: IO Bool
    -- ^ Waits until there is something to accept.  'False' instead once
    -- the server has been told to stop.
    }

-- | The listener for a socket an accept loop is about to run on.
--
-- The loop is stopped by being told, not by having the socket closed under
-- it.  A close ends a thread waiting on a descriptor only because the
-- event manager tells the waiter so, and that is being removed; after it
-- is, a waiter left behind on a number which has been handed to someone
-- else since would accept on that someone else's socket.
--
-- Where the wait cannot be made -- a completion-based I\/O manager has
-- nothing to say about a socket being readable, and before network 3.2.2
-- there is no 'waitAndCancelReadSocketSTM' to ask at all -- the old way
-- stands: the shutdown action closes the socket and the loop ends on the
-- @EBADF@ that accept() then gives.
makeListener :: ServerSettings -> Socket -> IO Listener
#if !defined(mingw32_HOST_OS) && MIN_VERSION_network(3,2,2)
makeListener ServerSettings{..} sock = do
    stopping <- newTVarIO False
    settingsInstallShutdownHandler $ atomically $ writeTVar stopping True
    return Listener{waitAcceptable = closedIsStopped $ wait stopping}
  where
    wait stopping = do
        -- Cancelled however this ends, and not only when it ends because
        -- the socket became readable: an I/O manager built on an interface
        -- like io_uring holds a reference on the socket for as long as the
        -- poll it was asked for is outstanding, so a wait left behind by a
        -- server that has stopped is a socket that does not close.
        (acceptable, cancelWait) <- waitAndCancelReadSocketSTM sock
        flip E.finally cancelWait $
            atomically $
                ((check =<< readTVar stopping) $> False)
                    <|> (acceptable $> True)

-- Closing the listening socket is how a server was stopped before there
-- was anything to tell, and callers which do it are still out there.
-- Waiting on a descriptor that has been closed is an @EBADF@, under
-- whichever name the I\/O manager gives it, and it means what the shutdown
-- action means: stop accepting.
closedIsStopped :: IO Bool -> IO Bool
closedIsStopped = E.handle $ \e ->
    if ioeGetErrorType e `elem` [InvalidArgument, UnsupportedOperation]
        then return False
        else E.throwIO e
#else
makeListener ServerSettings{..} sock = do
    settingsInstallShutdownHandler $ close sock
    return Listener{waitAcceptable = return True}
#endif

----------------------------------------------------------------

-- | Accepting a connection, retrying on transient errors.
--
-- 'accept' fails routinely for reasons which do not mean that the
-- listening socket is broken: the peer may reset the connection before
-- it is accepted (@ECONNABORTED@), or the process or the system may
-- have run out of file descriptors (@EMFILE@\/@ENFILE@).  Letting
-- these escape would terminate the accept loop, so they are retried
-- here.  Errors which do suggest a broken listening socket (@EBADF@,
-- @EINVAL@, ...) are re-thrown; a loop stops by being told, through
-- 'settingsInstallShutdownHandler', and 'makeListener' says why.
--
-- Running out of file descriptors is passed to 'settingsOnException'
-- since a server which keeps hitting it is effectively out of service.
-- @ECONNABORTED@ and @EINTR@ are not, being routine.
--
-- This function is interruptible: a blocked or sleeping retry still
-- receives asynchronous exceptions, so the server remains killable.
safeAccept :: ServerSettings -> Socket -> IO (Socket, SockAddr)
safeAccept set sock = safeAcceptWith set $ accept sock

-- | 'safeAccept' with the accepting action passed explicitly.  The
-- error paths described above cannot be provoked on a real listening
-- socket, so the test suite reaches them through this.
safeAcceptWith
    :: ServerSettings -> IO (Socket, SockAddr) -> IO (Socket, SockAddr)
safeAcceptWith set@ServerSettings{..} accept' = loop
  where
    loop = do
        ex <- E.try accept'
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
