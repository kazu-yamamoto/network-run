module Main (main) where

import Test.Hspec

import qualified CoreSpec
import qualified TCPSpec
import qualified TimeoutSpec
import qualified UDPSpec

main :: IO ()
main = hspec $ do
    describe "Network.Run.Core" CoreSpec.spec
    describe "Network.Run.TCP" TCPSpec.spec
    describe "Network.Run.TCP.Timeout" TimeoutSpec.spec
    describe "Network.Run.UDP" UDPSpec.spec
