{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import Ka.App (App (..), kaApp)
import qualified Ka.Graph as G
import Ka.Plugin (renderDoc)
import Ka.Route
import qualified Ka.View as View
import Main.Utf8 (withUtf8)
import Reflex
import Reflex.Dom
import Reflex.Dom.Pandoc (PandocBuilder)
import System.Directory (withCurrentDirectory)
import System.IO (BufferMode (LineBuffering), hSetBuffering)

notesDir :: FilePath
notesDir = "/home/srid/Sync/zk"

main :: IO ()
main =
  withUtf8 $
    withCurrentDirectory notesDir $ do
      hSetBuffering stdout LineBuffering
      mainWidgetWithHead
        headWidget
        bodyWidget

headWidget :: DomBuilder t m => m ()
headWidget = do
  View.headWidget
  el "title" $ text "ka Jsaddle"

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
        routeHist <- foldDyn (:) mempty nextRoute
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
  Dynamic t [Route] ->
  Route ->
  m (Event t Route)
renderRoute App {..} routeHist r = do
  evt0 <- divClass "ui mini steps" $ do
    switchHold never <=< dyn $
      ffor (reverse <$> routeHist) $ \rh -> fmap leftmost $
        forM rh $ \rPrev -> do
          elClass "a" "step" $ do
            divClass "content" $ do
              divClass "title" $ do
                -- TODO: clicking on this should discard later items in stack
                routeLink rPrev $ case rPrev of
                  Route_Main -> text "Main"
                  Route_Node th -> text $ G.unThing th
  evt1 <- case r of
    Route_Main -> do
      switchHold never <=< dyn $
        ffor (Map.keys <$> _app_doc) $ \fs -> do
          fmap leftmost $
            forM fs $ \fp -> do
              el "li" $ do
                routeLink (Route_Node fp) $ text $ G.unThing fp
    Route_Node fp -> do
      switchHold never <=< dyn $
        ffor (zipDyn _app_graph $ fmap (Map.lookup fp) _app_doc) $ \(g, v) -> case v of
          Nothing -> text "404" >> pure never
          Just thingData ->
            renderDoc g fp thingData
  evt2 <- divClass "ui center aligned basic segment" $ do
    routeLink Route_Main $
      text "Index"
  pure $ leftmost [evt0, evt1, evt2]
