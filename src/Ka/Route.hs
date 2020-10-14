module Ka.Route
  ( Route (..),
    style,
    routeLink,
    routeLinkWithAttr,
    renderRouteText,
    renderThingLink,
  )
where

import Clay ((?))
import qualified Clay as C
import Control.Lens.Operators
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Window as Window
import Ka.Graph (ThingName (..))
import Reflex.Dom

data Route
  = Route_Main
  | Route_Node ThingName
  deriving (Eq, Show)

-- TODO: This should scroll to top *after* route switch
routeLink ::
  forall js t m.
  ( DomBuilder t m,
    PostBuild t m,
    Prerender js t m
  ) =>
  Route ->
  m () ->
  m (Event t Route)
routeLink r w = do
  routeLinkWithAttr r (constDyn $ "class" =: "route") w

routeLinkWithAttr ::
  forall js t m.
  ( DomBuilder t m,
    PostBuild t m,
    Prerender js t m
  ) =>
  Route ->
  Dynamic t (Map AttributeName Text) ->
  m () ->
  m (Event t Route)
routeLinkWithAttr r attr w = do
  attrE <- dynamicAttributesToModifyAttributes attr
  let cfg =
        (def :: ElementConfig EventResult t (DomBuilderSpace m))
          & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
          -- & elementConfig_initialAttributes .~ attr
          & elementConfig_modifyAttributes .~ attrE
  (e, _a) <- element "a" cfg w
  let clicked = domEvent Click e
  scrollToTop clicked
  pure $ r <$ clicked

renderRouteText :: DomBuilder t m => Route -> m ()
renderRouteText = \case
  Route_Main -> elClass "i" "home icon" blank
  Route_Node t -> text $ unThingName t

renderThingLink ::
  ( Prerender js t m,
    PostBuild t m,
    DomBuilder t m
  ) =>
  ThingName ->
  m (Event t Route)
renderThingLink x = do
  let r = Route_Node x
  routeLink r $ renderRouteText r

style :: C.Css
style = do
  let linkColor = C.purple
  "a.route" ? do
    C.important $ do
      C.fontWeight C.bold
      C.color linkColor
      C.cursor C.pointer
  "a.route:hover" ? do
    C.important $ do
      C.textDecoration C.underline

scrollToTop :: forall m t js. (Prerender js t m, Monad m) => Event t () -> m ()
scrollToTop e = prerender_ blank $
  performEvent_ $
    ffor e $ \_ ->
      DOM.liftJSM $
        DOM.currentWindow >>= \case
          Nothing -> pure ()
          Just win -> Window.scrollTo win 0 0
