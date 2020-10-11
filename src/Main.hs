{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Ka.App (App (..), kaApp)
import Ka.Route
import Main.Utf8 (withUtf8)
import Reflex
import Reflex.Dom
import Reflex.Dom.Pandoc (PandocBuilder)
import System.Directory (withCurrentDirectory)

notesDir :: FilePath
notesDir = "/home/srid/Sync/zk"

main :: IO ()
main =
  withUtf8 $
    withCurrentDirectory notesDir $ do
      mainWidgetWithHead
        (el "title" $ text "ka Jsaddle")
        bodyWidget

bodyWidget ::
  ( PandocBuilder t m,
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
  app <- kaApp
  text "We will show file updates here"
  rec route :: Dynamic t Route <-
        holdDyn Route_Main switches
      switches <- switchHold never <=< dyn $
        ffor (traceDyn "route:" route) $ \r -> renderRoute app r
  pure ()

renderRoute ::
  ( DomBuilder t m,
    MonadHold t m,
    PostBuild t m
  ) =>
  App t m ->
  Route ->
  m (Event t Route)
renderRoute App {..} = \case
  Route_Main -> do
    switchHold never <=< dyn $
      ffor (Map.keys <$> _app_render) $ \fs -> do
        fmap leftmost $
          forM fs $ \fp -> do
            el "li" $ do
              e <- clickEvent $ el' "a" $ text $ T.pack fp
              pure $ ffor e $ \() -> Route_Node fp
  Route_Node fp -> do
    switchHold never <=< dyn $
      ffor (Map.lookup fp <$> _app_render) $ \case
        Nothing -> text "404" >> pure never
        Just w -> w
