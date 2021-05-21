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
                componentRef <- newIORef "" 
                let env = EnvIO {
                            _logger = writeIORef ref,
                            _loggerComponent = Logger (\_ -> writeIORef componentRef)
                        }
                runReaderT logic env
                msg <- readIORef ref
                assertEqual "log output" "this is a message" msg
                componentMsg <- readIORef componentRef
                assertEqual "component log output" "this is another message" componentMsg
         in testCase "run logic" $ test
    ]

main :: IO ()
main = defaultMain tests
