{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Djan.World
  ( -- * Types
    BlogPost (..),
    Blog (..),

    -- * Load
    loadBlog,
  )
where

import Dhall (FromDhall, Generic, auto, inputFile)
import Relude

data BlogPost where
  BlogPost ::
    { content :: Text,
      title :: Text,
      filename :: Text,
      tags :: [Text],
      authors :: [Text]
    } ->
    BlogPost
  deriving (Show, Generic, FromDhall)

data Blog where
  Blog :: {posts :: [BlogPost]} -> Blog
  deriving (Show, Generic, FromDhall)

loadBlog :: IO Blog
loadBlog = inputFile (auto @Blog) "./index.dhall"
