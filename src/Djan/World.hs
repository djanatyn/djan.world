{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Djan.World
  ( -- * Types
    BlogPost (..),
    Blog (..),

    -- * Render
    renderPage,
  )
where

import Relude
import Text.Blaze.Html5 (Html)

data BlogPost where
  BlogPost ::
    { content :: Text,
      title :: Text,
      filename :: Text,
      tags :: [Text],
      authors :: [Text]
    } ->
    BlogPost
  deriving (Show)

data Blog where
  Blog :: {posts :: [BlogPost]} -> Blog
  deriving (Show)

renderPage :: BlogPost -> IO Html
renderPage = undefined
