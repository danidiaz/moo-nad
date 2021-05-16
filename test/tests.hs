module Main where

import Test.Tasty
import Test.Tasty.HUnit

import LogicThatLogs
import Moo

tests :: TestTree
tests =
  testGroup
    "All"
    [ 
        let test = runReaderT logic (EnvIO putStrLn)
         in testCase "run logic" $ test
    ]

main :: IO ()
main = defaultMain tests
