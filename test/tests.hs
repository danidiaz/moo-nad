module Main where

import Test.Tasty
import Test.Tasty.HUnit

import LogicThatLogs

tests :: TestTree
tests =
  testGroup
    "All"
    [ 
        testCase "run logic" $ logic
    ]

main :: IO ()
main = defaultMain tests
