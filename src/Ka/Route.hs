{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Ka.Route
  ( R,
    pattern (:/),
    DSum ((:=>)),
    Route (..),
    style,
    routeLink,
    dynRouteLink,
    renderRouteText,
    renderThingLink,
  )
where

import Clay ((?))
import qualified Clay as C
import Control.Lens.Operators ((%~))
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.GADT.Compare.TH (DeriveGEQ (deriveGEq))
import Data.GADT.Show.TH (deriveGShow)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Window as Window
import Ka.Graph (ThingName (..))
import Ka.Scope (ThingScope, showScope)
import Reflex.Dom

type R f = DSum f Identity

-- | Convenience builder for an 'R' using 'Identity' for the functor.
pattern (:/) :: f a -> a -> R f
pattern a :/ b = a :=> Identity b

{-# COMPLETE (:/) #-}

infixr 5 :/

data Route a where
  Route_Node :: Route ThingName
  Route_Scope :: Route ThingScope

routeLink ::
  forall js t m.
  ( DomBuilder t m,
    PostBuild t m,
    Prerender js t m
  ) =>
  R Route ->
  m () ->
  m (Event t (R Route))
routeLink r =
  dynRouteLink (constDyn r) (constDyn $ "class" =: "route")

dynRouteLink ::
  forall js t m.
  ( DomBuilder t m,
    PostBuild t m,
    Prerender js t m
  ) =>
  Dynamic t (R Route) ->
  Dynamic t (Map AttributeName Text) ->
  m () ->
  m (Event t (R Route))
dynRouteLink rDyn attr w = do
  attrE <- dynamicAttributesToModifyAttributes attr
  let cfg =
        (def :: ElementConfig EventResult t (DomBuilderSpace m))
          & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const preventDefault)
          -- & elementConfig_initialAttributes .~ attr
          & elementConfig_modifyAttributes .~ attrE
  (e, _a) <- element "a" cfg w
  let clicked = domEvent Click e
  scrollToTop clicked
  pure $ tag (current rDyn) clicked

renderRouteText :: DomBuilder t m => R Route -> m ()
renderRouteText = \case
  Route_Node :/ t -> text $ unThingName t
  Route_Scope :/ ss -> text $ showScope ss

renderThingLink ::
  ( Prerender js t m,
    PostBuild t m,
    DomBuilder t m
  ) =>
  ThingName ->
  m (Event t (R Route))
renderThingLink x = do
  let r = Route_Node :/ x
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

$(deriveGShow ''Route)
$(deriveGEq ''Route)
$(deriveArgDict ''Route)