{-# LANGUAGE OverloadedStrings #-}

-- Echo client program
module Main (main) where

import qualified Data.ByteString.Char8 as C
import Network.Run.UDP (runUDPClient)
import Network.Socket
import Network.Socket.ByteString (recvFrom, sendTo)

main :: IO ()
main = runUDPClient "127.0.0.1" "3000" $ \sock sockAddr -> do
    -- Initially the local port is 0
    my1 <- getSocketName sock
    putStrLn $ "My sock addr " ++ show my1
    putStrLn $ "Peer sock addr " ++ show sockAddr
    _ <- sendTo sock "Hello, world!" sockAddr
    -- After sendTo, the local port is implicitly bound
    my2 <- getSocketName sock
    putStrLn $ "My sock addr " ++ show my2
    (msg, peer) <- recvFrom sock 1024
    putStrLn $ "Peer sock addr " ++ show peer
    putStr "Received: "
    C.putStrLn msg
