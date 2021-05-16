module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Data.IORef

import LogicThatLogs
import Moo

tests :: TestTree
tests =
  testGroup
    "All"
    [ 
        let test = do
                ref <- newIORef "" 
                runReaderT logic (EnvIO (writeIORef ref))
                msg <- readIORef ref
                assertEqual "log output" "this is another message" msg
         in testCase "run logic" $ test
    ]

main :: IO ()
main = defaultMain tests
