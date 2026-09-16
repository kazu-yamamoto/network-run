{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UDPSpec (spec) where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import Data.IORef
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Network.Socket
import Network.Socket.ByteString
import System.Timeout (timeout)
import Test.Hspec

import Network.Run.UDP

import Helper

spec :: Spec
spec = do
    describe "runUDPServer" $
        it "serves a datagram" $
            limited $ do
                port <- freeUDPPort
                let server sock = forever $ do
                        (bs, peer) <- recvFrom sock 2048
                        void $ sendTo sock bs peer
                withServerThread (runUDPServer (Just loopback) port server) $
                    udpRequest loopback port "hello" `shouldReturn` Just "hello"

    describe "runUDPServerFork" $ do
        it "serves a datagram" $
            limited $
                withUDPServerFork defaultServerSettings [loopback] echoDatagram $ \port ->
                    udpRequest loopback port "hello" `shouldReturn` Just "hello"

        it "returns at once when no host is given" $ limited $ do
            r <- timeout 1000000 $ runUDPServerFork [] "0" $ \_ _ -> return ()
            r `shouldBe` Just ()

        it "keeps serving after a handler throws" $ limited $ do
            (set, getReports) <- collecting
            ref <- newIORef (0 :: Int)
            let server sock bs = do
                    n <- atomicModifyIORef' ref $ \n -> (n + 1, n)
                    if n == 0
                        then ioError $ userError "boom"
                        else sendAll sock bs
            withUDPServerFork set [loopback] server $ \port -> do
                udpRequest loopback port "hello" `shouldReturn` Just "hello"
                reports <- waitFor 1 getReports
                case reports of
                    [] -> expectationFailure "the failure was not reported"
                    (mpeer, _) : _ -> mpeer `shouldSatisfy` isJust

        it "serves every given host" $ limited $ do
            ok <- hasIPv6
            if not ok
                then pendingWith "no IPv6 on this machine"
                else withUDPServerFork
                    defaultServerSettings
                    [loopback, "::1"]
                    echoDatagram
                    $ \port -> do
                        udpRequest loopback port "v4" `shouldReturn` Just "v4"
                        udpRequest "::1" port "v6" `shouldReturn` Just "v6"

        it "does not leak sockets" $
            limited $
                withUDPServerFork defaultServerSettings [loopback] echoDatagram $ \port -> do
                    -- Warming up first, so that one-off descriptors of the
                    -- runtime are not counted.
                    replicateM_ 5 $ udpRequest loopback port "warm"
                    threadDelay 200000
                    mbefore <- openFds
                    replicateM_ 40 $ udpRequest loopback port "hello"
                    threadDelay 300000
                    mafter <- openFds
                    case (mbefore, mafter) of
                        (Just n0, Just n1) ->
                            n1 - n0 `shouldSatisfy` (< 10)
                        _ -> pendingWith "file descriptors are not observable here"

    describe "runUDPClient" $
        it "closes the socket when the action throws" $
            limited $ do
                port <- freeUDPPort
                ref <- newIORef Nothing
                let action = runUDPClient loopback port $ \sock _ -> do
                        writeIORef ref $ Just sock
                        E.throwIO $ userError "boom"
                (action :: IO ()) `shouldThrow` anyIOException
                msock <- readIORef ref
                case msock of
                    Nothing -> expectationFailure "the socket was never opened"
                    Just sock -> do
                        r <- E.try $ getSocketOption sock ReuseAddr
                        case r :: Either E.IOException Int of
                            Left _ -> return ()
                            Right _ -> expectationFailure "the client socket is still open"

----------------------------------------------------------------

echoDatagram :: Socket -> ByteString -> IO ()
echoDatagram sock bs = sendAll sock bs

-- | A UDP port which is free at the time of the call.  Unlike a TCP
-- server, a UDP server here binds the port itself, so it cannot be
-- discovered afterwards.
freeUDPPort :: IO ServiceName
freeUDPPort = do
    addr <- resolve Datagram (Just loopback) "0" [AI_PASSIVE] NE.head
    E.bracket (openServerSocket addr) close $ \sock ->
        show . portOf <$> getSocketName sock

withUDPServerFork
    :: ServerSettings
    -> [HostName]
    -> (Socket -> ByteString -> IO ())
    -> (ServiceName -> IO a)
    -> IO a
withUDPServerFork set hosts server body = do
    port <- freeUDPPort
    withServerThread (runUDPServerForkWithSettings set hosts port server) $ do
        threadDelay 200000
        body port

-- | Sending a datagram until a reply comes back.  UDP may drop it, and
-- the server may not have bound its port yet.
udpRequest :: HostName -> ServiceName -> ByteString -> IO (Maybe ByteString)
udpRequest host port bs = runUDPClient host port $ \sock server -> go (20 :: Int) sock server
  where
    go 0 _ _ = return Nothing
    go n sock server = do
        void $ sendTo sock bs server
        mr <- timeout 200000 $ fst <$> recvFrom sock 2048
        case mr of
            Just r -> return $ Just r
            Nothing -> go (n - 1) sock server
