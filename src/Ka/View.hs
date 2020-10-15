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
import Ka.Breadcrumb (Breadcrumbs)
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
      Thing.style
      ".main" ? do
        Route.style

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
  divClass "ui two column grid container" $ do
    app <- kaApp
    rec route :: Dynamic t Route <-
          holdDyn Route_Main nextRoute
        routeHist <- foldDyn Breadcrumb.putCrumb (Breadcrumb.init Route_Main) nextRoute
        nextRoute <- switchHold never <=< dyn $
          ffor (traceDyn "route" route) $ \r -> renderRoute app routeHist r
    pure ()

renderRoute ::
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
  Dynamic t (Breadcrumbs Route) ->
  Route ->
  m (Event t Route)
renderRoute App {..} routeHist r = do
  evt0 <- divClass "four wide column" $ do
    Breadcrumb.render routeHist
  divClass "twelve wide left floated left aligned main column" $ do
    evt1 <- divClass "ui basic segment" $ case r of
      Route_Main -> do
        switchHold never <=< dyn $
          ffor (Map.keys <$> _app_doc) $ \fs -> do
            fmap leftmost $
              forM fs $ \fp -> do
                el "li" $ do
                  routeLink (Route_Node fp) $ text $ unThingName fp
      Route_Node fp -> do
        let thingDyn = fmap (Map.lookup fp) _app_doc
        thingDynM <- maybeDyn thingDyn
        switchHold never <=< dyn $
          ffor thingDynM $ \case
            Nothing -> text "404" >> pure never
            Just thingDataM ->
              Thing.render _app_graph fp thingDataM
    pure $ leftmost [evt0, evt1]
