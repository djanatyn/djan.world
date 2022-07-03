{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Djan.World
  ( HomePage (..),
    loadIcons,
    renderHomepage,
  )
import Relude

-- | Site homepage.
home :: HomePage
home =
  HomePage
    { recentPosts = [],
      projects = []
    }

main :: IO ()
main = do
  icons <- loadIcons
  putText $ renderHomepage icons home
