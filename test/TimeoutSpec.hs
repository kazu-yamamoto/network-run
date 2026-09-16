{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TimeoutSpec (spec) where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Data.IORef
import Network.Socket
import Network.Socket.ByteString
import qualified System.TimeManager as T
import System.Timeout (timeout)
import Test.Hspec

import Network.Run.Core (ServerSettings (..), defaultServerSettings)
import qualified Network.Run.TCP.Timeout as Timeout

import Helper

spec :: Spec
spec = do
    describe "runTCPServer" $ do
        it "serves a connection" $
            limited $
                withTimeoutServer defaultServerSettings 2 echoServer $ \port ->
                    request port "hello" `shouldReturn` "hello"

        it "kills a handler which exceeds the timeout" $ limited $ do
            let server _ _ sock = do
                    threadDelay 8000000
                    sendAll sock "late"
            withTimeoutServer defaultServerSettings 1 server $ \port ->
                client port $ \sock -> do
                    sendAll sock "hello"
                    (bs, ms) <- elapsed $ recv sock 1024
                    -- The handler was killed, so the connection is
                    -- closed rather than answered.
                    bs `shouldBe` ""
                    ms `shouldSatisfy` (< 5000)

        it "keeps a handler which tickles alive" $ limited $ do
            -- The handler lives 2.4 seconds, longer than the timeout,
            -- but it tickles every 300ms.  The margin between the two
            -- is what a loaded machine may eat without the test
            -- becoming a lie, so it is kept wide.
            let server _ th sock = do
                    replicateM_ 8 $ threadDelay 300000 >> T.tickle th
                    sendAll sock "ok"
            withTimeoutServer defaultServerSettings 2 server $ \port ->
                client port $ \sock -> do
                    sendAll sock "hello"
                    recv sock 1024 `shouldReturn` "ok"

        it "keeps serving after a handler throws" $ limited $ do
            (set, getReports) <- collecting
            ref <- newIORef (0 :: Int)
            let server _ _ sock = do
                    n <- atomicModifyIORef' ref $ \n -> (n + 1, n)
                    if n == 0 then E.throwIO (userError "boom") else echo sock
            withTimeoutServer set 2 server $ \port -> do
                ignoreAny $ request port "hello"
                request port "hello" `shouldReturn` "hello"
                reports <- waitFor 1 getReports
                map fst reports `shouldSatisfy` all (/= Nothing)

        it "stops when the listening socket is closed" $ limited $ do
            done <- newEmptyMVar
            withListenSocket $ \lsock _ -> do
                void $
                    forkFinally
                        (Timeout.runTCPServerWithSocket 2 lsock echoServer)
                        (putMVar done)
                threadDelay 100000
                close lsock
                r <- timeout 2000000 $ takeMVar done
                case r of
                    Just (Left _) -> return ()
                    Just (Right _) -> expectationFailure "the accept loop returned"
                    Nothing -> expectationFailure "the accept loop did not stop"

----------------------------------------------------------------

echoServer :: Timeout.TimeoutServer ()
echoServer _ _ sock = echo sock

withTimeoutServer
    :: ServerSettings
    -> Int
    -> Timeout.TimeoutServer ()
    -> (PortNumber -> IO a)
    -> IO a
withTimeoutServer set tm server body = withListenSocket $ \lsock port ->
    withServerThread
        (Timeout.runTCPServerWithSocketAndSettings set tm lsock server)
        (body port)
