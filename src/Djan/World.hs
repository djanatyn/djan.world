{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Djan.World
  ( -- * Types
    HomePage (..),
    BlogPost (..),
    Project (..),
    Icons (..),

    -- * Load Assets
    loadIcons,

    -- * Front Page (HomePage)
    buildHomepage,
    renderHomepage,
  )
where

import Relude
import qualified Text.Blaze as T
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

-- | Icons used on the site. All Icons are from Ionicons:
-- |
-- | <https://ionic.io/ionicons>
data Icons where
  Icons :: {planet :: H.Html} -> Icons

-- | Load icons.
loadIcons :: IO Icons
loadIcons = do
  planet <-
    H.preEscapedToHtml <$> readFileText "assets/ionicons/planet-outline.svg"

  pure $ Icons {planet}

-- | Body element using Bulma monospace font family helper.
-- |
-- | <https://bulma.io/documentation/helpers/typography-helpers/#font-family>
monospacedBody :: H.Html -> H.Html
monospacedBody content = H.body content H.! A.class_ "is-family-monospace"

-- | Navigation bar. Contains a planet icon.
navBar :: Icons -> H.Html
navBar Icons {planet} =
  ( H.nav $ do
      ( H.div $ do
          H.a planet ! A.class_ "navbar-item"
          H.a "home" ! A.class_ "navbar-item" ! A.href "TODO"
          H.a "about" ! A.class_ "navbar-item" ! A.href "TODO"
          H.a "blog" ! A.class_ "navbar-item" ! A.href "TODO"
        )
        ! A.class_ "navbar-brand"
  )
    ! A.class_ "navbar"
    ! A.role "navigation"
    ! T.customAttribute "aria-label" "main navigation"
  where
    (!) = (H.!)

-- | Introductory header for front page.
welcomeHeader :: H.Html
welcomeHeader = div $ do
  H.p "howdy!"
  H.p "my name is Jonathan Strickland. i also go by {djan} and {djanatyn}."
  H.p "i'm queer and non-binary. i use they/them pronouns!"
  H.p $ do
    "i love programming! i'm"
    H.a "recurse center" ! A.href "https://www.recurse.com/about"
    "alumni."
  H.p $ do
    "i main peach in super smash bros melee :) i enter a lot of "
    H.a "netplay tournaments" ! A.href "https://www.start.gg/user/e666c731"
    "on smash.gg."
  H.p "this site is the output of a haskell program."
  where
    div content = H.div content ! A.class_ "content"
    (!) = (H.!)

-- | Load Bulma 0.9.4 stylesheet from cdn.jsdelivr.net.
bulmaStylesheet :: H.Html
bulmaStylesheet =
  H.link
    H.! A.rel "stylesheet"
    H.! A.href "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"

-- | Build front page html, given a set of recent posts and highlighted projects.
buildHomepage :: Icons -> HomePage -> H.Html
buildHomepage icons (HomePage {recentPosts, projects}) = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta
      ! A.name "viewport"
      ! A.content "width=device-width, initial-scale=1"
    bulmaStylesheet
    H.title "djan.world"
  monospacedBody $ do
    navBar icons
    section $
      columns $
        column $
          container $ do
            H.h1 "djan.world" ! A.class_ "title"
            welcomeHeader
  where
    section content = H.section content ! A.class_ "section"
    columns content = H.div content ! A.class_ "columns"
    column content = H.div content ! A.class_ "column" ! A.class_ "is-10" ! A.class_ "is-offset-1"
    container content = H.div content ! A.class_ "container"
    (!) = (H.!)

-- | Render HomePage as Text.
-- |
-- | This function loads SVG icons and embeds them in the document.
renderHomepage :: Icons -> HomePage -> Text
renderHomepage icons home = toText . renderHtml $ buildHomepage icons home
