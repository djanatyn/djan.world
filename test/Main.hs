{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Djan.World as World
import Relude
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testCase "render homepage" $ do
        icons <- loadIcons
        print $ renderHomepage icons (HomePage { recentPosts = [], projects = [] })
    ]
