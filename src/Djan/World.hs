{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Djan.World
  ( -- * Types
    HomePage(..),
    BlogPost,
    Project,

    -- HomePage
    buildHomepage,
    renderHomepage,
  )
where

import Relude
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as SA

-- | Front page.
data HomePage where
  HomePage ::
    { recentPosts :: [BlogPost],
      projects :: [Project]
    } ->
    HomePage
  deriving (Show)

data BlogPost where
  BlogPost ::
    { content :: FilePath,
      slug :: Text,
      title :: Text,
      filename :: Text,
      tags :: [Text],
      authors :: [Text]
    } ->
    BlogPost
  deriving (Show)

data Project where
  Project ::
    { title :: Text,
      link :: Text,
      description :: Text,
      tags :: [Text]
    } ->
    Project
  deriving (Show)

-- | Planet icon, from ionicons.
-- |
-- | <https://ionic.io/ionicons>
loadPlanet :: IO H.Html
loadPlanet = H.preEscapedToHtml <$> readFileText "assets/ionicons/planet-outline.svg"

-- | Body element using Bulma monospace font family helper.
-- |
-- | <https://bulma.io/documentation/helpers/typography-helpers/#font-family>
monospacedBody :: H.Html -> H.Html
monospacedBody content = H.body content H.! A.class_ "is-family-monospace"

-- | Build front page html, given a set of recent posts and highlighted projects.
buildHomepage :: HomePage -> IO H.Html
buildHomepage HomePage {recentPosts, projects} = do
  planet <- loadPlanet
  pure $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta
        ! A.name "viewport"
        ! A.content "width=device-width, initial-scale=1"
      H.title "djan.world"
      H.link
        ! A.rel "stylesheet"
        ! A.href "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"
    monospacedBody $ do
      H.p "hi"
      planet
  where
    (!) = (H.!)

-- | Render HomePage as Text.
-- |
-- | This function loads SVG icons and embeds them in the document.
renderHomepage :: HomePage -> IO Text
renderHomepage home = buildHomepage home >>= pure . toText . renderHtml
