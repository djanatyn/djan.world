-- |
module Main where

import Djan.World
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [testCase "load blog" $ loadBlog >> pure ()]
