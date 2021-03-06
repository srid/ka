{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ka.PandocView
  ( render,
    style,
  )
where

import Clay (Css, pct, px, (?))
import qualified Clay as C
import qualified Data.Text as T
import Ka.Markdown (mdFileThing)
import Ka.Route (R, Route (..), renderThingLink)
import Reflex.Dom.Core
import Reflex.Dom.Pandoc
  ( Config (Config),
    PandocBuilder,
    PandocRaw (..),
    elPandoc,
    elPandocRawSafe,
  )
import Text.Pandoc.Definition (Pandoc)

instance PandocRaw (HydrationDomBuilderT s t m) where
  type PandocRawConstraints (HydrationDomBuilderT s t m) = (DomBuilder t (HydrationDomBuilderT s t m))
  elPandocRaw = elPandocRawSafe

-- | Route monoid for use with reflex-dom-pandoc
newtype RouteM t = RouteM {unRouteM :: Event t (R Route)}

instance Reflex t => Semigroup (RouteM t) where
  RouteM a <> RouteM b = RouteM $ leftmost [a, b]

instance Reflex t => Monoid (RouteM t) where
  mempty = RouteM never

render ::
  ( PandocBuilder t m,
    PostBuild t m,
    MonadHold t m,
    Prerender js t m
  ) =>
  Dynamic t Pandoc ->
  m (Event t (R Route))
render doc = do
  switchHold never <=< dyn $
    ffor doc $ fmap unRouteM . elPandoc pandocCfg
  where
    pandocCfg = Config $ \f url _inlines ->
      fmap RouteM $ do
        -- Treat everything but normal URLs as a wiki link (to local Markdown
        -- file)
        if "://" `T.isInfixOf` url
          then f >> pure never
          else renderThingLink $ mdFileThing (toString url)

style :: Css
style = do
  ".pandoc" ? do
    -- Make list items more "relaxed" (with spacing around them)
    "ul li" ? do
      C.paddingTop $ C.em 0.3
    -- GH checkboxes should have padding to right, to prevent text from "sticking"
    "li .ui.checkbox, .task .ui.checkbox" ? do
      C.important $ C.marginRight $ C.em 0.3
    "#footnotes" ? do
      C.fontSize $ pct 85
      C.borderTop C.solid (px 1) C.black
