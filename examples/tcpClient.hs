{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Char8 as C
import Network.Run.TCP (runTCPClient)
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = runTCPClient "127.0.0.1" "3000" $ \s -> do
    sendAll s "Hello, world!"
    msg <- recv s 1024
    putStr "Received: "
    C.putStrLn msg
