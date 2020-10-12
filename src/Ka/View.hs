{-# LANGUAGE RecursiveDo #-}

module Ka.View
  ( headWidget,
    bodyWidget,
  )
where

import Clay (Css, render, (?))
import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import Ka.App (App (..), kaApp)
import qualified Ka.Breadcrumb as Breadcrumb
import Ka.Graph (ThingName (unThingName))
import Ka.Route (Route (..), routeLink)
import qualified Ka.Route as Route
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
      ".ui.container" ? do
        Route.style
        Thing.style

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
  divClass "ui text container" $ do
    app <- kaApp
    rec route :: Dynamic t Route <-
          holdDyn Route_Main nextRoute
        routeHist <- foldDyn Breadcrumb.pushCrumb (one Route_Main) nextRoute
        nextRoute <- switchHold never <=< dyn $
          ffor (traceDyn "route" route) $ \r -> renderRoute app routeHist r
    pure ()

renderRoute ::
  ( PandocBuilder t m,
    MonadHold t m,
    PostBuild t m,
    Prerender js t m
  ) =>
  App t ->
  Dynamic t (NonEmpty Route) ->
  Route ->
  m (Event t Route)
renderRoute App {..} routeHist r = do
  evt0 <- Breadcrumb.render routeHist
  evt1 <- case r of
    Route_Main -> do
      switchHold never <=< dyn $
        ffor (Map.keys <$> _app_doc) $ \fs -> do
          fmap leftmost $
            forM fs $ \fp -> do
              el "li" $ do
                routeLink (Route_Node fp) $ text $ unThingName fp
    Route_Node fp -> do
      switchHold never <=< dyn $
        ffor (zipDyn _app_graph $ fmap (Map.lookup fp) _app_doc) $ \(g, v) -> case v of
          Nothing -> text "404" >> pure never
          Just thingData ->
            Thing.render g fp thingData
  evt2 <- divClass "ui center aligned basic segment" $ do
    routeLink Route_Main $
      text "Index"
  pure $ leftmost [evt0, evt1, evt2]
