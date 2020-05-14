{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Run.Core (
    resolve
  , openSocket
  , openServerSocket
  ) where

import qualified Control.Exception as E
import Network.Socket

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
openServerSocket addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    withFdSocket sock $ setCloseOnExecIfNeeded
    bind sock $ addrAddress addr
    return sock
