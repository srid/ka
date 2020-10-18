module Ka.Sidebar.Breadcrumb
  ( Breadcrumbs,
    init,
    putCrumb,
    render,
  )
where

import Control.Monad.Fix (MonadFix)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Ka.Route (Route (..), dynRouteLink, renderRouteText)
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

init :: a -> Breadcrumbs a
init x =
  Breadcrumbs [] x []

-- | Put a new crumb, such that it behaves like the stacked navigation of
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
  Dynamic t Route ->
  m (Event t Route)
render r = do
  routeHist <-
    foldDyn
      putCrumb
      (init Route_Main)
      (updated r)
  fmap (switch . current . fmap leftmost) $ do
    routeHistL <- holdUniqDyn $ fmap toList routeHist
    currentCrumb <- holdUniqDyn $ fmap _breadcrumbs_current routeHist
    simpleList routeHistL $ \crumb -> do
      -- TODO: Do this properly using GADT and factorDyn
      hackR <- maybeDyn $
        ffor crumb $ \case
          Route_Main -> Nothing
          Route_Node th -> Just th
      let itemClass = ffor2 currentCrumb crumb $ \curr x ->
            "class" =: bool "item" "active purple item" (curr == x)
      switchHold never <=< dyn $
        ffor hackR $ \case
          -- Route_Main
          Nothing -> do
            dynRouteLink (constDyn Route_Main) itemClass $ do
              divClass "content" $ do
                elClass "i" "home icon" blank
                renderClock
          -- Route_Node
          Just rDyn -> do
            let hereR = Route_Node <$> rDyn
            dynRouteLink hereR itemClass $ do
              dyn_ $ ffor hereR $ renderRouteText

renderClock ::
  ( MonadIO m,
    MonadIO (Performable m),
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    PerformEvent t m,
    TriggerEvent t m,
    DomBuilder t m
  ) =>
  m ()
renderClock = do
  now <- liftIO getCurrentTime
  let secs = 1
  tick <- clockLossy secs now
  dyn_ $ renderTick <$> tick
  where
    renderTick TickInfo {..} =
      el "time" $ text $ toText $ formatTime defaultTimeLocale "%T" _tickInfo_lastUTC
