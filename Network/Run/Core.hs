{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Run.Core (
    runClient
  , resolve
  , openServerSocket
  ) where

import qualified Control.Exception as E
import Control.Monad (when)
import Network.Socket

runClient :: SocketType -> String -> String -> (Socket -> SockAddr -> IO a) -> IO a
runClient socketType host port client = withSocketsDo $ do
    addr <- resolve socketType (Just host) port False
    let sockAddr = addrAddress addr
#if MIN_VERSION_network(3,1,1)
    E.bracket (open addr) (\sock -> gracefulClose sock 5000)
                          (\sock -> client sock sockAddr)
#else
    E.bracket (open addr) close $ \sock -> client sock sockAddr
#endif
  where
    open addr = do
        sock <- openSocket addr
        when (socketType == Stream) $ connect sock $ addrAddress addr
        return sock

resolve :: SocketType -> Maybe HostName -> ServiceName -> Bool -> IO AddrInfo
resolve socketType mhost port passive =
        head <$> getAddrInfo (Just hints) mhost (Just port)
  where
    hints = defaultHints {
        addrSocketType = socketType
      , addrFlags = if passive then [AI_PASSIVE] else []
      }

openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

openServerSocket :: AddrInfo -> IO Socket
openServerSocket addr = do
    sock <- openSocket addr
    setSocketOption sock ReuseAddr 1
    withFdSocket sock $ setCloseOnExecIfNeeded
    bind sock $ addrAddress addr
    return sock
