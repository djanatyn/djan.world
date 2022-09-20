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

-- isTitle :: Node -> Bool
-- isTitle (Node {nodeName = Identifier "title"}) = True
-- isTitle _ = False

-- ghci> filter isTitle $ docNodes parsedMeta
-- [ Node
--   { nodeAnn = Nothing
--   , nodeName = Identifier "title"
--   , nodeArgs =
--     [ Value { valueAnn = Nothing
--             , valueExp = StringValue "Blog Post 1"
--             }
--     ]
--   , nodeProps = fromList []
--   , nodeChildren = []
--   }
-- ]

-- ghci> concatMap (fmap valueExp . nodeArgs) $ filter isTitle $ docNodes parsedMeta
-- [StringValue "Blog Post 1"]

-- ghci> post <- fromMaybe (error "failed") . viaNonEmpty tail . lines . toText <$> readFile "posts/nix-scripting.md"
-- ghci> meta = Data.Text.strip . unlines . fst $ L.splitAt (fromMaybe (error "failed") $ L.elemIndex (toText "---") post) post

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
    "i love"
    H.a "programming!" ! A.href "https://github.com/djanatyn"
    "i'm" >> H.a "recurse center" ! A.href "https://www.recurse.com/about" >> "alumni."
  H.p $ do
    "i main peach in super smash bros melee :) i enter a lot of "
    H.a "netplay tournaments" ! A.href "https://www.start.gg/user/e666c731"
    "on smash.gg."
  H.p $ do
    "this site is the output of a"
    H.a "haskell program." ! A.href "https://github.com/djanatyn/djan.world"
  where
    div content = H.div content ! A.class_ "content"
    (!) = (H.!)

blankPost :: BlogPost
blankPost =
  BlogPost
    { content = "...",
      slug = "...",
      title = "title",
      filename = "...",
      tags = ["...", "...", "..."],
      authors = ["..."]
    }

-- | Section for recent blog posts.
blogPostSection :: [BlogPost] -> [H.Html]
blogPostSection posts =
  let (!) = (H.!)
      box content = H.div content ! A.class_ "box"
      buildEntry (BlogPost {title, tags}) =
        box $ do
          H.a (H.toHtml title) ! A.href ""
          H.p "description of blog post"
          mapM_ (\tag -> H.span (H.toHtml tag) ! A.class_ "tag") tags
   in buildEntry <$> posts

-- | Section for projects.
-- <!-- projects -->
-- <h2>projects</h2>
-- <div class="box">
--   <a href="https://github.com/djanatyn/ssbm-nix">ssbm-nix</a>
--   <p>Nix expressions for Super Smash Bros. Melee players.</p>
--   <span class="tag">nix</span>
--   <span class="tag">super smash bros melee</span>
-- </div>
-- <div class="box">
--   <a href="https://github.com/djanatyn/melee-dat">melee-dat</a>
--   <p>Replace DAT files within a v1.02 NTSC GALE01 GCM disk image.</p>
--   <span class="tag">rust</span>
--   <span class="tag">super smash bros melee</span>
-- </div>
-- <div class="box">
--   <a href="https://github.com/djanatyn/resume">resume</a>
--   <p>work resume (built using nix flakes + dhall + haskell)</p>
--   <span class="tag">haskell</span>
--   <span class="tag">dhall</span>
--   <span class="tag">nix</span>
-- </div>
projectSection :: [Project] -> H.Html
projectSection = undefined

-- | Footer for git revision + credits.
-- <footer class="footer">
--   <div class="content has-text-centered">
--     <svg></svg>
--     <p>rev <strong>f603995031f5c12f4d924e6c05cf552d0de39bd7</strong> - Mon Apr 4 12:31:52 2022 -0400</p>
--     <p>icons used are from <a href="https://ionic.io/ionicons"> ionicons</a>. styled with <a href="https://bulma.io/">bulma</a>.</p>
--     <p></p>
--   </div>
-- </footer>
footerSection :: H.Html
footerSection = undefined

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
      columns $ do
        column $
          container $ do
            H.h1 "djan.world" ! A.class_ "title"
            welcomeHeader
            H.h2 "blog posts" ! A.class_ "title"
            H.div (sequence_ $ blogPostSection $ replicate 10 blankPost) ! A.class_ "content"
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
