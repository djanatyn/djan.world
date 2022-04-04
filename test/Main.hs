{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Djan.World
import Relude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase "load blog" $ loadBlog >> pure (),
      testCase "render every page" $ do
        Blog {posts} <- loadBlog
        traverse renderPage posts >> pure ()
    ]
