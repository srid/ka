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
import Ka.Breadcrumb (Breadcrumbs)
import qualified Ka.Breadcrumb as Breadcrumb
import qualified Ka.Plugin.Calendar as Calendar
import Ka.Route (Route (..))
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
      -- Get rid of gutters from grid columns
      ".grid" ? do
        ".column" ? do
          C.important $ do
            C.sym C.margin $ C.px 0
            C.paddingRight $ C.px 0
        ".column.main" ? do
          C.important $ do
            C.sym C.padding $ C.px 0
      ".navbar.column" ? do
        C.important $ do
          C.maxHeight $ C.vh 100
          C.overflow C.auto
      "body" ? do
        C.backgroundColor "#fcfcfc"

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
    divClass "ui two column grid" $ do
      app <- kaApp
      rec route :: Dynamic t Route <-
            holdDyn Route_Main nextRoute
          routeHist <- foldDyn Breadcrumb.putCrumb (Breadcrumb.init Route_Main) nextRoute
          nextRoute <- renderRoute app routeHist (traceDyn "route" route)
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
  Dynamic t Route ->
  m (Event t Route)
renderRoute App {..} routeHist r = do
  evt0 <- divClass "four wide navbar column" $ do
    let sidebarThings = filter Calendar.includeInSidebar . Map.keys <$> (traceDynWith (const "doc") _app_doc)
    Breadcrumb.render sidebarThings routeHist
  divClass "twelve wide main column" $ do
    -- TODO: Do this properly using GADT and factorDyn
    hackR <- maybeDyn $
      ffor r $ \case
        Route_Main -> Nothing
        Route_Node th -> Just th
    evt1 <- switchHold never <=< dyn $
      ffor hackR $ \case
        -- Route_Main
        Nothing -> do
          divClass "ui basic segment" $ do
            elClass "h1" "header" $ text "ka"
            let cnt = Map.size <$> _app_doc
            el "p" $ do
              text "Note count: "
              dynText $ show <$> cnt
            pure never
        -- Route_Node
        Just fpDyn -> do
          let thingDyn = zipDynWith (\fp doc -> (fp,) <$> Map.lookup fp doc) fpDyn _app_doc
          thingDynM <- maybeDyn thingDyn
          switchHold never <=< dyn $
            ffor thingDynM $ \case
              Nothing -> text "404" >> pure never
              Just thingDataM ->
                Thing.render _app_graph thingDataM
    pure $ leftmost [evt0, evt1]
