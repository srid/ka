module Ka.Sidebar where

import Control.Monad.Fix (MonadFix)
import Ka.Graph (ThingName (..))
import Ka.Route (Route (..))
import qualified Ka.Sidebar.Breadcrumb as Breadcrumb
import qualified Ka.Sidebar.Listing as Listing
import Reflex.Dom.Core

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
  Dynamic t [ThingName] ->
  Dynamic t Route ->
  m (Event t Route)
render ths r = do
  divClass "ui right floated small fluid inverted vertical menu nav" $ do
    fmap leftmost $
      sequence
        [ Breadcrumb.render r,
          Listing.render ths
        ]
