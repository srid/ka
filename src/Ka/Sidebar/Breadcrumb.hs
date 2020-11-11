{-# LANGUAGE PatternSynonyms #-}

module Ka.Sidebar.Breadcrumb
  ( Breadcrumbs,
    defaultRoute,
    init,
    putCrumb,
    render,
  )
where

import Control.Monad.Fix (MonadFix)
import Ka.Route
  ( DSum ((:=>)),
    R,
    Route (..),
    dynRouteLink,
    renderRouteText,
    pattern (:/),
  )
import Ka.Scope (noScope, showScope)
import Reflex.Dom.Core
import Prelude hiding (init)

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

defaultRoute :: R Route
defaultRoute =
  Route_Scope :/ noScope

init :: a -> Breadcrumbs a
init x =
  Breadcrumbs [] x []

-- | Put a new crumbR, such that it behaves like the stacked navigation of
-- https://notes.andymatuschak.org/
putCrumb :: Eq a => a -> Breadcrumbs a -> Breadcrumbs a
putCrumb x bc@(Breadcrumbs before curr _) =
  case break (== x) (toList bc) of
    (_, []) ->
      -- Discard 'after'
      Breadcrumbs (before <> [curr]) x []
    (before', _x : after') ->
      Breadcrumbs before' x after'

render ::
  forall t m js.
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    Prerender js t m,
    MonadIO (Performable m),
    MonadIO m,
    PerformEvent t m,
    TriggerEvent t m
  ) =>
  Dynamic t (R Route) ->
  m (Event t (R Route))
render r = do
  routeHist :: Dynamic t (Breadcrumbs (R Route)) <-
    foldDyn
      putCrumb
      (init defaultRoute)
      (updated r)
  routeHistL <- holdUniqDyn $ fmap toList routeHist
  currentCrumb <- holdUniqDyn $ fmap _breadcrumbs_current routeHist
  fmap (switch . current . fmap leftmost) $ do
    simpleList routeHistL $ \crumbR -> do
      let itemClass = ffor2 currentCrumb crumbR $ \curr x ->
            "class" =: bool "item" "active purple item" (curr == x)
      dynRouteLink crumbR itemClass $ do
        factored <- factorDyn crumbR
        dyn_ @t @m @() $
          ffor factored $ \case
            Route_Scope :=> (fmap runIdentity . getCompose -> scopeDyn) -> do
              dyn_ $ fmap (text . showScope) scopeDyn
            Route_Node :=> (fmap runIdentity . getCompose -> thDyn) -> do
              dyn_ $ renderRouteText . (Route_Node :/) <$> thDyn
