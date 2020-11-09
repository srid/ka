{-# LANGUAGE RecursiveDo #-}

module Ka.View
  ( headWidget,
    bodyWidget,
  )
where

import Clay (Css, render, (?))
import qualified Clay as C
import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import Ka.App (App (..), kaApp)
import Ka.Graph (ThingName (unThingName))
import qualified Ka.Plugin.Highlight as Highlight
import Ka.Route hiding (style)
import qualified Ka.Route as Route
import Ka.Scope (ThingScope, splitScope)
import Ka.ScopeView (renderScopeContents, renderScopeCrumbs)
import qualified Ka.Sidebar as Sidebar
import Ka.Sidebar.Breadcrumb (defaultRoute)
import qualified Ka.Thing as Thing
import Reflex.Dom.Core
import Reflex.Dom.Pandoc (PandocBuilder)

headWidget :: DomBuilder t m => m ()
headWidget = do
  elAttr "meta" ("http-equiv" =: "Content-Type" <> "content" =: "text/html; charset=utf-8") blank
  elAttr "meta" ("content" =: "width=device-width, initial-scale=1" <> "name" =: "viewport") blank
  elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: "https://cdn.jsdelivr.net/npm/fomantic-ui@2.8.7/dist/semantic.min.css") blank
  el "style" $ do
    text $ toStrict $ render style
  el "title" $ text "ka Jsaddle"
  where
    style :: Css
    style = do
      "body" ? do
        -- Force scrollbar
        C.overflowY C.scroll
        C.backgroundColor "#fcfcfc"
      gridStyle
      ".main" ? do
        Route.style
        Thing.style
        Highlight.style
    gridStyle = do
      -- Get rid of gutters from grid columns
      ".ka.grid > .row" ? do
        C.important $ do
          C.paddingBottom $ C.px 0
      ".ka.grid > .row > .column" ? do
        C.important $ do
          C.sym C.margin $ C.px 0
          C.paddingRight $ C.px 0
      ".ka.grid > .row .column.main" ? do
        C.important $ do
          C.padding 0 (C.em 0.4) 0 0
      ".ka.grid > .row .column.navbar" ? do
        stickyColumn

    stickyColumn = do
      C.important $ do
        C.maxHeight $ C.vh 100
        C.overflow C.auto
        C.position C.sticky
        -- FIXME: padding here is janky on Chrome
        -- C.paddingTop $ C.px 0
        C.top $ C.px 0

bodyWidget ::
  ( PandocBuilder t m,
    Prerender js t m,
    MonadIO m,
    PerformEvent t m,
    PostBuild t m,
    TriggerEvent t m,
    MonadHold t m,
    MonadFix m,
    MonadIO (Performable m)
  ) =>
  m ()
bodyWidget = do
  divClass "ui fluid container" $
    divClass "ui two column ka grid" $ do
      divClass "row" $ do
        app <- kaApp
        rec route :: Dynamic t (R Route) <-
              holdDyn defaultRoute nextRoute
            nextRoute <- renderRoute app (traceDyn "route" route)
        pure ()

renderRoute ::
  forall t m js.
  ( PandocBuilder t m,
    MonadHold t m,
    PostBuild t m,
    MonadFix m,
    Prerender js t m,
    MonadIO (Performable m),
    MonadIO m,
    PerformEvent t m,
    TriggerEvent t m
  ) =>
  App t ->
  Dynamic t (R Route) ->
  m (Event t (R Route))
renderRoute App {..} r = do
  evt0 <- divClass "four wide navbar column" $ do
    Sidebar.render (Map.keys <$> _app_doc) r
  evt1 <- divClass "twelve wide main column" $ do
    divClass "ui basic attached segments thing" $ do
      -- Scope breadcrumbs; REFACTOR
      mscope <- maybeDyn $
        ffor2 _app_doc r $ \docs -> \case
          Route_Scope :/ sc -> Just $ splitScope sc
          Route_Node :/ th -> case Map.lookup th docs of
            Nothing -> Nothing
            Just (scope, _) -> Just (Just scope, toString $ unThingName th)
      r0 <- switchHold never <=< dyn $
        ffor mscope $ \case
          Nothing -> pure never
          Just (split :: Dynamic t (Maybe ThingScope, FilePath)) -> do
            renderScopeCrumbs (fst <$> split) (toText . snd <$> split)
      -- Route body
      factored <- factorDyn r
      r1 <- switchHold never <=< dyn $
        ffor factored $ \case
          Route_Scope :=> (fmap runIdentity . getCompose -> scopeDyn) -> do
            renderScopeContents (fmap fst <$> _app_doc) scopeDyn
          Route_Node :=> (fmap runIdentity . getCompose -> fpDyn) -> do
            let thingDyn = zipDynWith (\fp doc -> (fp,) <$> Map.lookup fp doc) fpDyn _app_doc
            thingDynM <- maybeDyn thingDyn
            switchHold never <=< dyn $
              ffor thingDynM $ \case
                Nothing -> text "404" >> pure never
                Just thingDataM ->
                  Thing.render _app_graph _app_doc thingDataM
      pure $ leftmost [r0, r1]
  pure $ leftmost [evt0, evt1]
