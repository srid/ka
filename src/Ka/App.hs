module Ka.App where

import Commonmark (defaultSyntaxSpec)
import qualified Commonmark.Extensions as CE
import Control.Monad.Fix (MonadFix)
import Data.Dependent.Sum (DSum (..))
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime, getCurrentTime)
import Ka.Graph (Graph, ThingName)
import qualified Ka.Graph as G
import qualified Ka.Markdown as M
import qualified Ka.Plugin.Calendar as Calendar
import Ka.Plugin.Highlight (highlightSpec)
import Ka.Plugin.Tag (inlineTagSpec)
import qualified Ka.Plugin.Task as Task
import Ka.Plugin.WikiLink (wikiLinkSpec)
import Ka.Scope (ThingScope)
import qualified Ka.Scope as Scope
import Ka.Thing (Thing (..), ThingVal (..))
import Ka.Watch (directoryFilesContent)
import Reflex hiding (mapMaybe)
import Reflex.Dom.Pandoc (PandocBuilder)
import Text.Pandoc.Definition (Pandoc)

data App t = App
  { _app_graph :: Dynamic t Graph,
    _app_things :: Dynamic t (Map ThingName Thing)
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
  fileContentE <- directoryFilesContent "." M.noteExtension
  logDiffEvent fileContentE
  let pandocWithScopeE :: Event t (Map ThingName (Maybe (ThingScope, (UTCTime, Pandoc)))) =
        -- Discard the parent paths; we only consider the basename to be note identifier.
        ffor (Scope.diffMapScoped <$> fileContentE) $ \m ->
          Map.mapKeys M.mdFileThing $
            flip Map.mapWithKey m $ \fp -> fmap $ \s ->
              let spec =
                    wikiLinkSpec
                      <> inlineTagSpec
                      <> highlightSpec
                      <> CE.gfmExtensions
                      <> CE.fancyListSpec
                      <> CE.footnoteSpec
                      <> CE.smartPunctuationSpec
                      <> CE.definitionListSpec
                      <> CE.attributesSpec
                      <> CE.fencedDivSpec -- Used for publishing (semantic UI classes)
                      <> defaultSyntaxSpec
               in second (M.parseMarkdown spec fp) <$> s
      pandocE :: Event t (Map ThingName (Maybe Pandoc)) = fmap (fmap $ snd . snd) <$> pandocWithScopeE
  graphD :: Dynamic t Graph <-
    foldDyn G.patch G.empty $
      ffor pandocE $
        Map.map $
          fmap $ fmap (swap . first M.mdFileThing) . Map.toList . M.queryNoteLinksWithContext
  pandocD :: Dynamic t (Map ThingName (ThingScope, (UTCTime, Pandoc))) <-
    foldDyn G.patchMap mempty pandocWithScopeE
  -- NOTE: If two plugins produce the same file, the later plugin's output will
  -- be used, discarding the formers. That is what `flip Map.union` effectively does.
  currTime <- liftIO getCurrentTime
  renderE <-
    mergeWith (flip Map.union)
      <$> sequence
        -- TODO: Eventually create a proper Plugin type to hold these functions.
        [ (fmap . fmap . fmap . fmap . fmap . fmap) (\x -> ThingVal_Pandoc :=> Identity x) $ pure pandocWithScopeE,
          (fmap . fmap . fmap . fmap . fmap) (\x -> (currTime, ThingVal_Calendar :=> Identity x)) $ Calendar.runPlugin graphD pandocD pandocWithScopeE,
          (fmap . fmap . fmap . fmap . fmap) (\x -> (currTime, ThingVal_Tasks :=> Identity x)) $ Task.runPlugin graphD pandocD pandocWithScopeE
        ]
  docD <-
    foldDyn G.patchMap mempty $ fmap (fmap $ \(sc, (mt, v)) -> Thing sc mt v) <$> renderE
  pure $ App graphD docD

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
