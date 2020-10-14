module Ka.Breadcrumb where

import Control.Monad.Fix (MonadFix)
import Ka.Route (Route, renderRouteText, routeLinkWithAttr)
import Reflex.Dom.Core

-- | A non empty list with cursor
data Breadcrumbs a = Breadcrumbs
  { _breadcrumbs_before :: [a],
    _breadcrumbs_current :: a,
    _breadcrumbs_after :: [a]
  }
  deriving (Eq, Show)

instance Foldable Breadcrumbs where
  foldMap f (Breadcrumbs before curr after) =
    foldMap f before <> f curr <> foldMap f after

init :: a -> Breadcrumbs a
init x =
  Breadcrumbs [] x []

-- | Put a new crumb, such that it behaves like the stacked navigation of
-- https://notes.andymatuschak.org/
putCrumb :: Eq a => a -> Breadcrumbs a -> Breadcrumbs a
putCrumb x bc@(Breadcrumbs before curr _) =
  case break (== x) (toList bc) of
    (_, []) ->
      Breadcrumbs (before <> [curr]) x []
    (before', _x : after') ->
      Breadcrumbs before' x after'

render ::
  forall t m js.
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    Prerender js t m
  ) =>
  Dynamic t (Breadcrumbs Route) ->
  m (Event t Route)
render routeHist = do
  divClass "ui basic segment" $
    divClass "ui right floated inverted vertical menu" $ do
      evt <- fmap (switch . current . fmap leftmost) $
        simpleList (toList <$> routeHist) $ \rPrevD -> switchHold never <=< dyn $
          ffor (zipDyn rPrevD $ _breadcrumbs_current <$> routeHist) $ \(rPrev, rCurr) -> do
            let itemClass = bool "item" "active purple item" $ rPrev == rCurr
            routeLinkWithAttr rPrev (constDyn $ "class" =: itemClass) $ do
              divClass "content" $ do
                divClass "title" $ do
                  renderRouteText rPrev
      void $
        divClass "item" $ do
          divClass "ui input" $ do
            inputElement def
      pure evt