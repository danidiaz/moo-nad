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
                loggerRef <- newIORef "" 
                counterRef <- newIORef 0
                let env = EnvIO {
                              _logger = \_ -> writeIORef loggerRef
                            , _counter = Counter {
                                   askCounter = readIORef counterRef
                                 , incCounter = \v -> modifyIORef counterRef (+v)
                                 }
                        }
                runReaderT logic env
                msg <- readIORef loggerRef
                assertEqual "log output" "this is a message" msg
                counterVal <- readIORef counterRef
                assertEqual "counter output" 1 counterVal
         in testCase "run logic" $ test
    ]

main :: IO ()
main = defaultMain tests
