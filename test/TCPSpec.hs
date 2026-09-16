{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TCPSpec (spec) where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import Data.IORef
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import GHC.IO.Exception (IOErrorType (InvalidArgument))
import Network.Socket
import Network.Socket.ByteString
import System.IO.Error (ioeGetErrorType)
import System.Info (os)
import System.Timeout (timeout)
import Test.Hspec

import Network.Run.Core (openServerSocket)
import Network.Run.TCP

import Helper

spec :: Spec
spec = do
    describe "runTCPServer" $ do
        it "serves a connection" $
            limited $
                withTCPServer defaultServerSettings echo $ \port ->
                    request port "hello" `shouldReturn` "hello"

        it "resolves and binds the address itself" $ limited $ do
            -- The port cannot be chosen in advance here, so an
            -- ephemeral one is looked up first.
            port <- freeTCPPort
            withServerThread (runTCPServer (Just loopback) (show port) echo) $ do
                threadDelay 200000
                request port "hello" `shouldReturn` "hello"

        it "keeps serving after a handler throws" $ limited $ do
            (set, getReports) <- collecting
            ref <- newIORef (0 :: Int)
            withTCPServer set (failFirst ref) $ \port -> do
                ignoreAny $ request port "hello"
                request port "hello" `shouldReturn` "hello"
                reports <- waitFor 1 getReports
                length reports `shouldBe` 1

        it "reports the peer of a failed handler" $ limited $ do
            (set, getReports) <- collecting
            withTCPServer set (\_ -> E.throwIO $ userError "boom") $ \port -> do
                peer <- client port $ \sock -> do
                    sendAll sock "hello"
                    getSocketName sock
                reports <- waitFor 1 getReports
                map fst reports `shouldBe` [Just peer]

        it "keeps serving when the exception hook itself throws" $ limited $ do
            let set =
                    defaultServerSettings
                        { settingsOnException = \_ _ -> E.throwIO $ userError "hook"
                        }
            ref <- newIORef (0 :: Int)
            withTCPServer set (failFirst ref) $ \port -> do
                ignoreAny $ request port "hello"
                request port "hello" `shouldReturn` "hello"

        it "stops when the listening socket is closed" $ limited $ do
            done <- newEmptyMVar
            withListenSocket $ \lsock _ -> do
                void $
                    forkFinally (runTCPServerWithSocket lsock echo) (putMVar done)
                threadDelay 100000
                close lsock
                r <- timeout 2000000 $ takeMVar done
                case r of
                    Just (Left _) -> return ()
                    Just (Right _) -> expectationFailure "the accept loop returned"
                    Nothing -> expectationFailure "the accept loop did not stop"

        it "listens on a single address family" $ limited $ do
            port <- freeTCPPort
            let hints =
                    defaultHints
                        { addrSocketType = Stream
                        , addrFlags = [AI_PASSIVE]
                        }
            addrs <- getAddrInfo (Just hints) (Just "localhost") (Just $ show port)
            let families = NE.toList $ NE.map addrFamily addrs
            if length (nub families) < 2
                then pendingWith "localhost has a single address family here"
                else do
                    -- Only the first address is used, so the other
                    -- family is not served at all.
                    let (served, unserved)
                            | addrFamily (NE.head addrs) == AF_INET6 =
                                ("::1", "127.0.0.1")
                            | otherwise = ("127.0.0.1", "::1")
                    withServerThread (runTCPServer (Just "localhost") (show port) echo) $ do
                        threadDelay 200000
                        echoOn served port `shouldReturn` "hello"
                        echoOn unserved port `shouldThrow` anyIOException

        it "drains a connection when the graceful close timeout is positive" $ limited $ do
            -- The handler returns while the request it never read is
            -- still queued.  'gracefulClose' sends FIN and drains it.
            gate <- newEmptyMVar
            let set = defaultServerSettings{settingsGracefulCloseTimeout = 500}
            withTCPServer set (\_ -> takeMVar gate) $ \port ->
                client port $ \sock -> do
                    sendAll sock "hello"
                    threadDelay 200000
                    putMVar gate ()
                    recv sock 1024 `shouldReturn` ""

        it "resets a connection when the graceful close timeout is not positive" $ limited $ do
            -- The same, with 'close' instead: unread data in the queue
            -- makes the kernel answer with RST.
            gate <- newEmptyMVar
            let set = defaultServerSettings{settingsGracefulCloseTimeout = 0}
            withTCPServer set (\_ -> takeMVar gate) $ \port ->
                client port $ \sock -> do
                    sendAll sock "hello"
                    threadDelay 200000
                    putMVar gate ()
                    recv sock 1024 `shouldThrow` anyIOException

        it "serves many connections concurrently" $
            limited $
                withTCPServer defaultServerSettings echo $ \port -> do
                    vars <- replicateM 50 newEmptyMVar
                    forM_ vars $ \var -> forkIO $ do
                        r <-
                            (Just <$> request port "hello")
                                `E.catch` \(_ :: E.SomeException) -> return Nothing
                        putMVar var r
                    rs <- mapM takeMVar vars
                    rs `shouldBe` replicate 50 (Just "hello")

    describe "openTCPServerSocket" $ do
        it "listens, unlike openServerSocket" $ limited $ do
            addr <- resolve Stream (Just loopback) "0" [AI_PASSIVE] NE.head
            -- Probing with 'connect' instead would be slow: BSD drops
            -- the SYN sent to a socket which is bound but does not
            -- listen, where Linux answers with RST.
            E.bracket (openServerSocket addr) close $ \sock ->
                accept sock `shouldThrow` invalidArgument
            withTCPServer defaultServerSettings echo $ \port ->
                request port "hello" `shouldReturn` "hello"

        it "sets ReuseAddr" $ limited $ withListenSocket $ \lsock _ ->
            getSocketOption lsock ReuseAddr `shouldNotReturn` 0

        it "sets close-on-exec" $ limited $ withListenSocket $ \lsock _ ->
            withFdSocket lsock getCloseOnExec `shouldReturn` True

        it "sets a composite option" $ limited $ do
            addr <- resolve Stream (Just loopback) "0" [AI_PASSIVE] NE.head
            let opts = [(Linger, SockOptValue $ StructLinger 1 0)]
            E.bracket (openTCPServerSocketWithOpts opts addr) close $ \lsock -> do
                StructLinger onoff _ <- getSockOpt lsock Linger
                onoff `shouldBe` 1

        it "makes an IPv6 socket IPv6 only" $ limited $ onIPv6 $ do
            addr <- resolve Stream (Just "::") "0" [AI_PASSIVE] NE.head
            E.bracket (openTCPServerSocket addr) close $ \lsock -> do
                getSocketOption lsock IPv6Only `shouldNotReturn` 0
                port <- portOf <$> getSocketName lsock
                withServerThread (void $ runTCPServerWithSocket lsock echo) $ do
                    -- The IPv4 loopback must not reach it.
                    r <- E.try $ request port "hello"
                    case r :: Either E.IOException ByteString of
                        Left _ -> return ()
                        Right _ -> expectationFailure "IPv4 reached an IPv6 only socket"

        it "can be asked for a dual stack socket" $
            limited $
                onIPv6 $
                    if os == "openbsd"
                        then pendingWith "OpenBSD always makes IPv6 sockets IPv6 only"
                        else do
                            addr <- resolve Stream (Just "::") "0" [AI_PASSIVE] NE.head
                            E.bracket
                                (openTCPServerSocketWithOptions [(IPv6Only, 0)] addr)
                                close
                                $ \lsock -> do
                                    port <- portOf <$> getSocketName lsock
                                    withServerThread
                                        (void $ runTCPServerWithSocket lsock echo)
                                        $ request port "hello" `shouldReturn` "hello"

    describe "runTCPClient" $ do
        it "opens the socket with settingsOpenClientSocket" $ limited $ do
            ref <- newIORef (0 :: Int)
            withTCPServer defaultServerSettings echo $ \port -> do
                let set =
                        defaultSettings
                            { settingsOpenClientSocket = \addr -> do
                                atomicModifyIORef' ref $ \n -> (n + 1, ())
                                openClientSocketWithOptions [(NoDelay, 1)] addr
                            }
                r <- runTCPClientWithSettings set loopback (show port) $ \sock -> do
                    sendAll sock "hello"
                    recv sock 1024
                r `shouldBe` "hello"
                readIORef ref `shouldReturn` 1

        it "connects to what settingsSelectAddrInfo chose" $
            limited $
                withTCPServer defaultServerSettings echo $ \port -> do
                    -- Resolving a port where nothing listens, and then
                    -- replacing the address with the real one.  Only a
                    -- client which honours the selection can connect.
                    let real = SockAddrInet port $ tupleToHostAddress (127, 0, 0, 1)
                        set =
                            defaultSettings
                                { settingsSelectAddrInfo = \ais ->
                                    (NE.head ais){addrAddress = real}
                                }
                        wrong = show (port + 1)
                    r <- runTCPClientWithSettings set loopback wrong $ \sock -> do
                        sendAll sock "hello"
                        recv sock 1024
                    r `shouldBe` "hello"

        it "closes the socket when the action throws" $ limited $ do
            ref <- newIORef Nothing
            withTCPServer defaultServerSettings echo $ \port -> do
                let set =
                        defaultSettings
                            { settingsOpenClientSocket = \addr -> do
                                sock <- openClientSocket addr
                                writeIORef ref $ Just sock
                                return sock
                            }
                    action = runTCPClientWithSettings set loopback (show port) $
                        \_ -> E.throwIO $ userError "boom"
                (action :: IO ()) `shouldThrow` anyIOException
                msock <- readIORef ref
                case msock of
                    Nothing -> expectationFailure "the socket was never opened"
                    Just sock -> do
                        r <- E.try $ getSocketOption sock ReuseAddr
                        case r :: Either E.IOException Int of
                            Left _ -> return ()
                            Right _ -> expectationFailure "the client socket is still open"

    describe "resolve" $ do
        it "asks for a passive wildcard address" $ do
            addr <- resolve Stream Nothing "0" [AI_PASSIVE] NE.head
            addrSocketType addr `shouldBe` Stream
            case addrAddress addr of
                SockAddrInet _ ha -> ha `shouldBe` tupleToHostAddress (0, 0, 0, 0)
                SockAddrInet6 _ _ ha _ ->
                    ha `shouldBe` tupleToHostAddress6 (0, 0, 0, 0, 0, 0, 0, 0)
                sa -> expectationFailure $ "unexpected address: " ++ show sa

        it "uses the given socket type" $ do
            addr <- resolve Datagram (Just loopback) "0" [] NE.head
            addrSocketType addr `shouldBe` Datagram

        it "returns what the selector chose" $ do
            let mark = SockAddrInet 1 $ tupleToHostAddress (1, 2, 3, 4)
            addr <- resolve Stream (Just loopback) "0" [] $ \ais ->
                (NE.head ais){addrAddress = mark}
            addrAddress addr `shouldBe` mark

----------------------------------------------------------------

-- | 'accept' on a socket which is not listening fails with @EINVAL@.
invalidArgument :: Selector E.IOException
invalidArgument e = ioeGetErrorType e == InvalidArgument

-- | A handler which fails the first connection and echoes the rest.
failFirst :: IORef Int -> Socket -> IO ()
failFirst ref sock = do
    n <- atomicModifyIORef' ref $ \n -> (n + 1, n)
    if n == 0 then E.throwIO (userError "boom") else echo sock

-- | One request and one response to the given host.
echoOn :: HostName -> PortNumber -> IO ByteString
echoOn host port = runTCPClient host (show port) $ \sock -> do
    sendAll sock "hello"
    recv sock 1024

-- | A TCP port which is free at the time of the call.
freeTCPPort :: IO PortNumber
freeTCPPort = withListenSocket $ \_ port -> return port

onIPv6 :: IO () -> IO ()
onIPv6 action = do
    ok <- hasIPv6
    if ok then action else pendingWith "no IPv6 on this machine"
