{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ka.View
  ( headWidget,
    renderThingLink,
    renderPandoc,
  )
where

import Clay (em, pct, px, (?))
import qualified Clay as C
import qualified Data.Text as T
import qualified Ka.Graph as G
import Ka.Markdown (mdFileThing)
import Ka.Route (Route (..), routeLink)
import Reflex.Dom.Core
import Reflex.Dom.Pandoc
  ( Config (Config),
    PandocBuilder,
    PandocRaw (..),
    elPandoc,
    elPandocRawSafe,
  )
import Text.Pandoc.Definition (Pandoc)

style :: C.Css
style = do
  ".ui.container" ? do
    let linkColor = C.purple
    "a.route" ? do
      C.important $ do
        C.fontWeight C.bold
        C.color linkColor
        C.cursor C.pointer
    "a.route:hover" ? do
      C.important $ do
        C.backgroundColor linkColor
        C.color C.white
    ".backlinks" ? do
      let smallerFont x = C.important $ C.fontSize x
      C.backgroundColor "#eee"
      "h2" ? smallerFont (em 1.2)
      "h3" ? smallerFont (pct 90)
      ".context" ? smallerFont (pct 85)
      C.color C.gray
      do
        let linkColorFaded = "#555"
        C.a ? do
          C.important $ do
            C.color linkColorFaded
        C.a C.# C.hover ? do
          C.important $ do
            C.backgroundColor linkColorFaded
            C.color C.white
    -- Pandoc styles
    "#footnotes" ? do
      C.fontSize $ pct 85
      C.borderTop C.solid (px 1) C.black

headWidget :: DomBuilder t m => m ()
headWidget = do
  elAttr "meta" ("http-equiv" =: "Content-Type" <> "content" =: "text/html; charset=utf-8") blank
  elAttr "meta" ("content" =: "width=device-width, initial-scale=1" <> "name" =: "viewport") blank
  elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: "https://cdn.jsdelivr.net/npm/fomantic-ui@2.8.7/dist/semantic.min.css") blank
  el "style" $ do
    text $ toStrict $ C.render style

renderThingLink :: (Prerender js t m, DomBuilder t m) => G.Thing -> m (Event t Route)
renderThingLink x = do
  routeLink (Route_Node x) $ do
    text $ G.unThing x

-- Pandoc rendering
-- ----------------

instance PandocRaw (HydrationDomBuilderT s t m) where
  type PandocRawConstraints (HydrationDomBuilderT s t m) = (DomBuilder t (HydrationDomBuilderT s t m))
  elPandocRaw = elPandocRawSafe

-- | Route monoid for use with reflex-dom-pandoc
newtype RouteM t = RouteM
  {unRouteM :: Event t Route}

instance Reflex t => Semigroup (RouteM t) where
  RouteM a <> RouteM b = RouteM $ leftmost [a, b]

instance Reflex t => Monoid (RouteM t) where
  mempty = RouteM never

renderPandoc :: (PandocBuilder t m, Prerender js t m) => Pandoc -> m (Event t Route)
renderPandoc doc = do
  fmap unRouteM $ elPandoc pandocCfg doc
  where
    pandocCfg = Config $ \f url _inlines ->
      fmap RouteM $ do
        if "://" `T.isInfixOf` url
          then f >> pure never
          else renderThingLink $ mdFileThing (toString url)
