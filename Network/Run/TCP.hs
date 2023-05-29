{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- | Simple functions to run TCP clients and servers.
module Network.Run.TCP (
    runTCPClient
  , runTCPServer
  ) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Network.Socket
#if defined(mingw32_HOST_OS)
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Control.Exception
#endif

import Network.Run.Core

-- | Running a TCP client with a connected socket.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve Stream (Just host) port False
#if MIN_VERSION_network(3,1,1)
    E.bracket (open addr) (\sock -> gracefulClose sock 5000) client
#else
    E.bracket (open addr) close client
#endif
  where
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

-- | Running a TCP server with an accepted socket and its peer name.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve Stream mhost port True
    E.bracket (open addr) close loop
  where
    open addr = E.bracketOnError (openServerSocket addr) close $ \sock -> do
        listen sock 1024
        return sock
    loop sock = forever $ E.bracketOnError (windowsThreadBlockHack (accept sock)) (close . fst) $
        \(conn, _peer) ->
#if MIN_VERSION_network(3,1,1)
          void $ forkFinally (server conn) (const $ gracefulClose conn 5000)
#else
          void $ forkFinally (server conn) (const $ close conn)
#endif


#if defined(mingw32_HOST_OS)
windowsThreadBlockHack :: IO a -> IO a
windowsThreadBlockHack act = do
    var <- newEmptyMVar :: IO (MVar (Either Control.Exception.SomeException a))
    void . forkIO $ Control.Exception.try act >>= putMVar var
    res <- takeMVar var
    case res of
      Left  e -> Control.Exception.throwIO e
      Right r -> return r
#else
windowsThreadBlockHack :: IO a -> IO a
windowsThreadBlockHack = id
#endif
