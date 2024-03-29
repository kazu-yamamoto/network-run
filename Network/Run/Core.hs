{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Run.Core (
    resolve,
    openSocket,
    openServerSocket,
    gclose,
) where

import qualified Control.Exception as E
import Network.Socket

resolve :: SocketType -> Maybe HostName -> ServiceName -> Bool -> IO AddrInfo
resolve socketType mhost port passive =
    head <$> getAddrInfo (Just hints) mhost (Just port)
  where
    hints =
        defaultHints
            { addrSocketType = socketType
            , addrFlags = if passive then [AI_PASSIVE] else []
            }

#if !MIN_VERSION_network(3,1,2)
openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
#endif

-- | Open socket for server use
--
-- The socket is configured to
--
-- * allow reuse of local addresses (SO_REUSEADDR)
-- * automatically be closed during a successful @execve@ (FD_CLOEXEC)
-- * bind to the address specified
openServerSocket :: AddrInfo -> IO Socket
openServerSocket addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    withFdSocket sock $ setCloseOnExecIfNeeded
    bind sock $ addrAddress addr
    return sock

gclose :: Socket -> IO ()
#if MIN_VERSION_network(3,1,1)
gclose sock = gracefulClose sock 5000
#else
gclose = close
#endif
