{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Djan.World
  ( HomePage (..),
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
main = renderHomepage home >>= putText
