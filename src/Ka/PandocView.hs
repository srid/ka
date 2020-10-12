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
import Ka.Route (Route (..), renderThingLink)
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
newtype RouteM t = RouteM
  {unRouteM :: Event t Route}

instance Reflex t => Semigroup (RouteM t) where
  RouteM a <> RouteM b = RouteM $ leftmost [a, b]

instance Reflex t => Monoid (RouteM t) where
  mempty = RouteM never

render :: (PandocBuilder t m, Prerender js t m) => Pandoc -> m (Event t Route)
render doc = do
  fmap unRouteM $ elPandoc pandocCfg doc
  where
    pandocCfg = Config $ \f url _inlines ->
      fmap RouteM $ do
        if "://" `T.isInfixOf` url
          then f >> pure never
          else renderThingLink $ mdFileThing (toString url)

style :: Css
style = do
  "#footnotes" ? do
    C.fontSize $ pct 85
    C.borderTop C.solid (px 1) C.black
