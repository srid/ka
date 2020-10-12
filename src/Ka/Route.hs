module Ka.Route where

import Control.Lens.Operators
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Window as Window
import Ka.Graph (Thing)
import Reflex.Dom

data Route
  = Route_Main
  | Route_Node Thing
  deriving (Eq, Show)

-- TODO: This should scroll to top after route switch
routeLink ::
  forall js t m.
  ( DomBuilder t m,
    Prerender js t m
  ) =>
  Route ->
  m () ->
  m (Event t Route)
routeLink r w = do
  let cfg =
        (def :: ElementConfig EventResult t (DomBuilderSpace m))
          & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
          & elementConfig_initialAttributes .~ "class" =: "route"
  (e, _a) <- element "a" cfg w
  let clicked = domEvent Click e
  scrollToTop clicked
  pure $ r <$ clicked

scrollToTop :: forall m t js. (Prerender js t m, Monad m) => Event t () -> m ()
scrollToTop e = prerender_ blank $
  performEvent_ $
    ffor e $ \_ ->
      DOM.liftJSM $
        DOM.currentWindow >>= \case
          Nothing -> pure ()
          Just win -> Window.scrollTo win 0 0

-- | Get the click event on an element
--
-- Use as:
--   clickEvent $ el' "a" ...
clickEvent ::
  ( DomBuilder t m,
    HasDomEvent t target 'ClickTag
  ) =>
  m (target, a) ->
  m (Event t ())
clickEvent w =
  fmap (fmap (const ()) . domEvent Click . fst) w
