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
import qualified Ka.Plugin.Highlight as Highlight
import Ka.Route hiding (style)
import qualified Ka.Route as Route
import Ka.ScopeView (renderScopeBreadcrumb, renderScopeContents)
import qualified Ka.Sidebar as Sidebar
import Ka.Sidebar.Breadcrumb (defaultRoute)
import qualified Ka.Thing as Thing
import Reflex.Dom.Core
import Reflex.Dom.Pandoc (PandocBuilder)

headWidget :: DomBuilder t m => Text -> m ()
headWidget notebookId = do
  elAttr "meta" ("http-equiv" =: "Content-Type" <> "content" =: "text/html; charset=utf-8") blank
  elAttr "meta" ("content" =: "width=device-width, initial-scale=1" <> "name" =: "viewport") blank
  elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: "https://cdn.jsdelivr.net/npm/fomantic-ui@2.8.7/dist/semantic.min.css") blank
  el "style" $ do
    text $ toStrict $ render style
  el "title" $ text $ "ka 🧠 " <> notebookId
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
              holdUniqDyn =<< holdDyn defaultRoute nextRoute
            nextRoute <- renderRouteBody app route
        pure ()

renderRouteBody ::
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
renderRouteBody App {..} r = do
  evt0 <- divClass "four wide navbar column" $ do
    let recentlyUpdatedThings =
          ffor _app_things $
            fmap fst . sortOn (Down . snd) . Map.toList . Map.map Thing._thing_lastModified
    Sidebar.render recentlyUpdatedThings r
  evt1 <- divClass "twelve wide main column" $ do
    divClass "ui basic attached segments thing" $ do
      e0 <- renderScopeBreadcrumb r $ _app_things <&> fmap Thing._thing_scope
      r' <- factorDyn $ traceDyn "route" r
      e1 <- switchHold never <=< dyn $
        ffor r' $ \case
          Route_Scope :=> (fmap runIdentity . getCompose -> scopeDyn) -> do
            renderScopeContents (fmap Thing._thing_scope <$> _app_things) scopeDyn
          Route_Node :=> (fmap runIdentity . getCompose -> fpDyn) -> do
            let thingDyn = zipDynWith (\fp doc -> (fp,) <$> Map.lookup fp doc) fpDyn _app_things
            thingDynM <- maybeDyn thingDyn
            switchHold never <=< dyn $
              ffor thingDynM $ \case
                Nothing ->
                  text "404" >> pure never
                Just thingDataM ->
                  Thing.render _app_graph _app_things thingDataM
      pure $ leftmost [e0, e1]
  pure $ leftmost [evt0, evt1]
