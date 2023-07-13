module Main (main) where

import Control.Monad (forever, unless, void)
import qualified Data.ByteString as S
import Network.Run.UDP (runUDPServer)
import Network.Socket.ByteString (recvFrom, sendTo)

main :: IO ()
main = runUDPServer (Just "127.0.0.1") "3000" $ \sock -> forever $ do
    (msg, peer) <- recvFrom sock 2048
    unless (S.null msg) $ void $ sendTo sock msg peer
