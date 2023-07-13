{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Simple functions to run TCP clients and servers.
module Network.Run.TCP.Timeout (
    runTCPServer
  , TimeoutServer
  ) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Network.Socket
import qualified System.TimeManager as T

import Network.Run.Core

type TimeoutServer a =
    T.Manager
    -- ^ A global timeout manager
    -> T.Handle
    -- ^ A thread-local timeout handler
    -> Socket
    -- ^ A connected socket
    -> IO a

-- | Running a TCP server with an accepted socket and its peer name.
runTCPServer
    :: Int
    -- ^ Timeout in second.
    -> Maybe HostName
    -> ServiceName
    -> TimeoutServer a
    -> IO a
runTCPServer tm mhost port server = withSocketsDo $ do
    T.withManager (tm * 1000000) $ \mgr -> do
        addr <- resolve Stream mhost port True
        E.bracket (open addr) close $ loop mgr
  where
    open addr = E.bracketOnError (openServerSocket addr) close $ \sock -> do
        listen sock 1024
        return sock
    loop mgr sock = forever $ E.bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) ->
#if MIN_VERSION_network(3,1,1)
          void $ forkFinally (server' mgr conn) (const $ gracefulClose conn 5000)
#else
          void $ forkFinally (server' mgr conn) (const $ close conn)
#endif
    server' mgr conn = do
        th <- T.registerKillThread mgr $ return ()
        server mgr th conn

