{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | White box tests for the internals of "Network.Run.Core".
--
-- The transient 'accept' errors and the closer failures handled there
-- cannot be provoked through a real socket, so they are injected.
module CoreSpec (spec) where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad (void)
import Data.IORef
import Foreign.C.Error (Errno (..), eCONNABORTED)
import GHC.IO.Exception (
    IOErrorType (Interrupted, OtherError),
    IOException (..),
 )
import Network.Socket
import System.IO.Error (fullErrorType, illegalOperationErrorType, mkIOError)
import System.Timeout (timeout)
import Test.Hspec

import Network.Run.Core

import Helper

spec :: Spec
spec = do
    describe "safeAcceptWith" $ do
        it "retries after running out of file descriptors" $ limited $ do
            (set0, getReports) <- collecting
            let set = set0{settingsAcceptRetryDelay = 50000}
            withFakeAccept [emfile, emfile] $ \(accept', count) -> do
                ((_, peer), ms) <- elapsed $ safeAcceptWith set accept'
                peer `shouldBe` fakePeer
                count `shouldReturn` 3
                -- Two retries of 50ms each.
                ms `shouldSatisfy` (>= 90)
                reports <- getReports
                map fst reports `shouldBe` [Nothing, Nothing]

        it "retries EINTR at once, without reporting it" $ limited $ do
            (set0, getReports) <- collecting
            -- A delay which would be obvious if it were taken.
            let set = set0{settingsAcceptRetryDelay = 5000000}
            withFakeAccept [eintr, eintr] $ \(accept', count) -> do
                (_, ms) <- elapsed $ safeAcceptWith set accept'
                count `shouldReturn` 3
                ms `shouldSatisfy` (< 1000)
                getReports `shouldReturn` []

        it "retries ECONNABORTED at once, without reporting it" $ limited $ do
            (set0, getReports) <- collecting
            let set = set0{settingsAcceptRetryDelay = 5000000}
            withFakeAccept [aborted] $ \(accept', count) -> do
                (_, ms) <- elapsed $ safeAcceptWith set accept'
                count `shouldReturn` 2
                ms `shouldSatisfy` (< 1000)
                getReports `shouldReturn` []

        it "rethrows an error of the listening socket" $ limited $ do
            (set, getReports) <- collecting
            withFakeAccept [bad] $ \(accept', count) -> do
                safeAcceptWith set accept'
                    `shouldThrow` (\e -> ioeGetErrorType' e == ioeGetErrorType' bad)
                count `shouldReturn` 1
                getReports `shouldReturn` []

        it "is still killable while waiting to retry" $ limited $ do
            let set = defaultServerSettings{settingsAcceptRetryDelay = 5000000}
            withFakeAccept (repeat emfile) $ \(accept', _) -> do
                done <- newEmptyMVar
                tid <- forkFinally (void $ safeAcceptWith set accept') (putMVar done)
                threadDelay 100000
                killThread tid
                r <- timeout 1000000 $ takeMVar done
                case r of
                    Just (Left _) -> return ()
                    Just (Right _) -> expectationFailure "accept returned"
                    Nothing -> expectationFailure "the retry was not interruptible"

    describe "report" $ do
        it "swallows a synchronous exception of the hook" $ do
            let set =
                    defaultServerSettings
                        { settingsOnException = \_ _ -> E.throwIO $ userError "hook"
                        }
            report set Nothing (E.toException $ userError "boom")
                `shouldReturn` ()

        it "rethrows an asynchronous exception of the hook" $ do
            let set =
                    defaultServerSettings
                        { settingsOnException = \_ _ -> E.throwIO E.ThreadKilled
                        }
            report set Nothing (E.toException $ userError "boom")
                `shouldThrow` (== E.ThreadKilled)

    describe "forkWith" $ do
        it "reports an exception which escapes the action" $ limited $ do
            (set, getReports) <- collecting
            withDummySocket $ \sock -> do
                closed <- newEmptyMVar
                forkWith set (\_ -> putMVar closed ()) sock fakePeer $
                    E.throwIO $
                        userError "boom"
                takeMVar closed `shouldReturn` ()
                reports <- waitFor 1 getReports
                map fst reports `shouldBe` [Just fakePeer]

        it "closes the socket even when the hook throws" $ limited $ do
            let set =
                    defaultServerSettings
                        { settingsOnException = \_ _ -> E.throwIO $ userError "hook"
                        }
            withDummySocket $ \sock -> do
                closed <- newEmptyMVar
                forkWith set (\_ -> putMVar closed ()) sock fakePeer $
                    E.throwIO $
                        userError "boom"
                r <- timeout 1000000 $ takeMVar closed
                r `shouldBe` Just ()

        it "reports a failure of the closer" $ limited $ do
            (set, getReports) <- collecting
            withDummySocket $ \sock ->
                forkWith set (\_ -> ioError $ userError "close failed") sock fakePeer $
                    return ()
            reports <- waitFor 1 getReports
            case reports of
                [(mpeer, desc)] -> do
                    mpeer `shouldBe` Just fakePeer
                    desc `shouldContain` "close failed"
                _ -> expectationFailure $ "unexpected reports: " ++ show reports

    describe "gcloseWith" $ do
        it "waits for the FIN of the peer if the timeout is positive" $
            limited $
                withHeldConnection $ \sock -> do
                    let set = defaultServerSettings{settingsGracefulCloseTimeout = 500}
                    (_, ms) <- elapsed $ gcloseWith set sock
                    ms `shouldSatisfy` (>= 300)

        it "closes at once if the timeout is not positive" $
            limited $
                withHeldConnection $ \sock -> do
                    let set = defaultServerSettings{settingsGracefulCloseTimeout = 0}
                    (_, ms) <- elapsed $ gcloseWith set sock
                    ms `shouldSatisfy` (< 300)

----------------------------------------------------------------

fakePeer :: SockAddr
fakePeer = SockAddrInet 12345 $ tupleToHostAddress (127, 0, 0, 1)

-- | An 'accept' which fails with the given errors before succeeding,
-- together with the number of times it has been called.
withFakeAccept
    :: [IOError] -> ((IO (Socket, SockAddr), IO Int) -> IO a) -> IO a
withFakeAccept errs body = withDummySocket $ \sock -> do
    ref <- newIORef errs
    cnt <- newIORef (0 :: Int)
    let accept' = do
            atomicModifyIORef' cnt $ \n -> (n + 1, ())
            me <- atomicModifyIORef' ref $ \es -> case es of
                [] -> ([], Nothing)
                e : rest -> (rest, Just e)
            case me of
                Just e -> E.throwIO e
                Nothing -> return (sock, fakePeer)
    body (accept', readIORef cnt)

-- | A socket which is never connected, standing in for an accepted one.
withDummySocket :: (Socket -> IO a) -> IO a
withDummySocket = E.bracket (socket AF_INET Stream defaultProtocol) close

-- | The server side of a connection whose peer stays open and silent,
-- so that a graceful close has to wait for its timeout.
withHeldConnection :: (Socket -> IO a) -> IO a
withHeldConnection body = withListenSocket $ \lsock port -> do
    var <- newEmptyMVar
    withServerThread (accept lsock >>= putMVar var . fst) $
        client port $
            \_held -> takeMVar var >>= body

emfile, eintr, aborted, bad :: IOError
emfile = mkIOError fullErrorType "accept" Nothing Nothing
eintr = emfile{ioe_type = Interrupted}
aborted = emfile{ioe_type = OtherError, ioe_errno = Just connAborted}
  where
    Errno connAborted = eCONNABORTED
bad = mkIOError illegalOperationErrorType "accept" Nothing Nothing

ioeGetErrorType' :: IOError -> IOErrorType
ioeGetErrorType' = ioe_type
