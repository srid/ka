{-# LANGUAGE RecursiveDo #-}

module Ka.App where

import Commonmark (defaultSyntaxSpec)
import qualified Commonmark.Extensions as CE
import Control.Monad.Fix (MonadFix)
import Data.Dependent.Sum (DSum (..))
import qualified Data.Map.Strict as Map
import Ka.Graph (Graph)
import qualified Ka.Graph as G
import Ka.Markdown (mdFileThing, noteExtension, parseMarkdown, queryLinksWithContext)
import Ka.Plugin
import qualified Ka.Plugin.Calendar as Calendar
import Ka.Plugin.WikiLink (wikiLinkSpec)
import Ka.Watch (directoryFilesContent)
import Reflex hiding (mapMaybe)
import Reflex.Dom.Pandoc (PandocBuilder)
import Text.Pandoc.Definition (Pandoc)

data App t = App
  { _app_graph :: Dynamic t Graph,
    -- _app_pandoc :: Dynamic t (Map G.Thing Pandoc),
    _app_doc :: Dynamic t (Map G.Thing (DSum Doc Identity))
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
  let pandocE :: Event t (Map G.Thing (Maybe Pandoc)) =
        ffor fileContentE $ \m ->
          Map.mapKeys mdFileThing $
            flip Map.mapWithKey m $ \fp -> fmap $ \s ->
              let spec =
                    wikiLinkSpec
                      <> CE.gfmExtensions
                      <> CE.fancyListSpec
                      <> CE.footnoteSpec
                      <> CE.smartPunctuationSpec
                      <> CE.definitionListSpec
                      <> defaultSyntaxSpec
               in parseMarkdown spec fp s
  graphD :: Dynamic t Graph <-
    foldDyn G.patch G.empty $
      ffor pandocE $
        Map.map (fmap $ fmap (swap . first mdFileThing) . Map.toList . queryLinksWithContext)
  pandocD :: Dynamic t (Map G.Thing Pandoc) <-
    foldDyn G.patchMap mempty pandocE
  -- NOTE: If two plugins produce the same file, the later plugin's output will
  -- be used, discarding the formers. That is what `flip Map.union` effectively does.
  renderE <-
    fmap (mergeWith $ flip Map.union) $
      sequence
        -- TODO: Eventually create a proper Plugin type to hold these functions.
        [ pure $ (fmap . fmap . fmap) (\x -> Doc_Pandoc :=> Identity x) pandocE,
          (fmap . fmap . fmap . fmap) (\x -> Doc_Calendar :=> Identity x) $
            Calendar.runPlugin graphD pandocD pandocE
        ]
  renderD <-
    foldDyn G.patchMap mempty renderE
  pure $ App graphD renderD

logDiffEvent ::
  (PerformEvent t m, MonadIO (Performable m)) =>
  Event t (Map String (Maybe a)) ->
  m ()
logDiffEvent evt =
  performEvent_ $
    ffor evt $ \fsMap ->
      forM_ (Map.toList fsMap) $ \(fs, change) ->
        case change of
          Just _ ->
            liftIO $ putStr "W " >> putStrLn fs
          Nothing ->
            liftIO $ putStr "- " >> putStrLn fs
