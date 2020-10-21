{-# LANGUAGE RecursiveDo #-}

module Ka.App where

import Commonmark (defaultSyntaxSpec)
import qualified Commonmark.Extensions as CE
import Control.Monad.Fix (MonadFix)
import Data.Dependent.Sum (DSum (..))
import qualified Data.Map.Strict as Map
import Ka.Graph (Graph, ThingName)
import qualified Ka.Graph as G
import Ka.Markdown (mdFileThing, noteExtension, parseMarkdown, queryLinksWithContext)
import qualified Ka.Plugin.Calendar as Calendar
import qualified Ka.Plugin.Task as Task
import Ka.Plugin.WikiLink (wikiLinkSpec)
import Ka.Thing
import Ka.Watch (directoryFilesContent)
import Reflex hiding (mapMaybe)
import Reflex.Dom.Pandoc (PandocBuilder)
import System.FilePath (takeFileName)
import Text.Pandoc.Definition (Pandoc)

data App t = App
  { _app_graph :: Dynamic t Graph,
    -- _app_pandoc :: Dynamic t (Map ThingName Pandoc),
    _app_doc :: Dynamic t (Map ThingName (DSum Thing Identity))
  }

kaApp ::
  forall t m.
  ( Reflex t,
    MonadIO m,
    PerformEvent t m,
    PostBuild t m,
    TriggerEvent t m,
    MonadHold t m,
    MonadFix m,
    MonadIO (Performable m),
    PandocBuilder t m
  ) =>
  m (App t)
kaApp = do
  fileContentE <- directoryFilesContent "." noteExtension
  logDiffEvent fileContentE
  let pandocE :: Event t (Map ThingName (Maybe Pandoc)) =
        -- Discard the parent paths; we only consider the basename to be note identifier.
        -- TODO: Support file tree in sidebar listing.
        ffor (diffMapWithOnlyBaseFileName <$> fileContentE) $ \m ->
          Map.mapKeys mdFileThing $
            flip Map.mapWithKey m $ \fp -> fmap $ \s ->
              let spec =
                    wikiLinkSpec
                      <> CE.gfmExtensions
                      <> CE.fancyListSpec
                      <> CE.footnoteSpec
                      <> CE.smartPunctuationSpec
                      <> CE.definitionListSpec
                      <> CE.bracketedSpanSpec
                      <> defaultSyntaxSpec
               in parseMarkdown spec fp s
  graphD :: Dynamic t Graph <-
    foldDyn G.patch G.empty $
      ffor pandocE $
        Map.map $
          fmap $ fmap (swap . first mdFileThing) . Map.toList . queryLinksWithContext
  pandocD :: Dynamic t (Map ThingName Pandoc) <-
    foldDyn G.patchMap mempty pandocE
  -- NOTE: If two plugins produce the same file, the later plugin's output will
  -- be used, discarding the formers. That is what `flip Map.union` effectively does.
  renderE <-
    fmap (mergeWith $ flip Map.union) $
      sequence
        -- TODO: Eventually create a proper Plugin type to hold these functions.
        [ pure $ (fmap . fmap . fmap) (\x -> Thing_Pandoc :=> Identity x) pandocE,
          (fmap . fmap . fmap . fmap) (\x -> Thing_Calendar :=> Identity x) $
            Calendar.runPlugin graphD pandocD pandocE,
          (fmap . fmap . fmap . fmap) (\x -> Thing_Tasks :=> Identity x) $
            Task.runPlugin graphD pandocD pandocE
        ]
  docD <-
    foldDyn G.patchMap mempty renderE
  pure $ App graphD docD

-- | Drop the parent directories, retaining only the base name, on map keys.
--
-- On conflict, take the Just value discarding the Nothing. With two Just
-- values, pick the greater of them (per Map.mapKeysWith semantics); this is
-- irrelevant to our app domain however (so effectively, the behaviour when
-- there are simultaneous modifications to files with the same base name is
-- undefined).
diffMapWithOnlyBaseFileName :: Map FilePath (Maybe a) -> Map FilePath (Maybe a)
diffMapWithOnlyBaseFileName =
  Map.mapKeysWith f takeFileName
  where
    f Nothing Nothing = Nothing
    f Nothing x = x
    f x Nothing = x
    f x _y = x

logDiffEvent ::
  (PerformEvent t m, MonadIO (Performable m)) =>
  Event t (Map String (Maybe a)) ->
  m ()
logDiffEvent evt =
  performEvent_ $
    ffor evt $ \fsMap -> do
      forM_ (Map.toList fsMap) $ \(fs, change) ->
        case change of
          Just _ ->
            liftIO $ putStr "W " >> putStrLn fs
          Nothing ->
            liftIO $ putStr "- " >> putStrLn fs
