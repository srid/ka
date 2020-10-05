{-# LANGUAGE RecursiveDo #-}

module Ka.App where

import qualified Algebra.Graph.AdjacencyMap as AM
import Commonmark (defaultSyntaxSpec)
import qualified Commonmark.Extensions as CE
import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Ka.Graph (Graph)
import qualified Ka.Graph as G
import Ka.Markdown (noteExtension, noteFileTitle, parseMarkdown, queryLinks)
import qualified Ka.Plugin.ViewNote as ViewNote
import Ka.Plugin.WikiLink (wikiLinkSpec)
import Ka.Watch (directoryFilesContent)
import Reflex hiding (mapMaybe)
import Reflex.Dom.Core
import Reflex.Host.Headless (MonadHeadlessApp)
import Text.Pandoc.Definition (Pandoc)

kaApp ::
  forall t m.
  MonadHeadlessApp t m =>
  m (Event t (Map FilePath (Maybe (IO ByteString))))
kaApp = do
  fileContentE <- directoryFilesContent "." noteExtension
  let pandocE :: Event t (Map FilePath (Maybe Pandoc)) =
        ffor fileContentE $
          Map.mapWithKey $ \fp -> fmap $ \s ->
            let spec =
                  defaultSyntaxSpec
                    <> wikiLinkSpec
                    <> CE.footnoteSpec
                    <> CE.gfmExtensions
             in parseMarkdown spec fp s
  graphD :: Dynamic t Graph <-
    foldDyn G.patch G.empty $
      ffor pandocE $
        Map.map (fmap $ toList . queryLinks)
  pandocD :: Dynamic t (Map FilePath Pandoc) <-
    foldDyn patchMap mempty pandocE
  -- NOTE: If two plugins produce the same file, the later plugin's output will
  -- be used, discarding the formers. That is what `flip Map.union` effectively does.
  fmap (mergeWith $ flip Map.union) $
    sequence
      [ pluginViewNotes graphD pandocD pandocE,
        pluginCalendar graphD pandocD pandocE
      ]

-- TODO: Extract the below as plugins once FRP API stablizes.

pluginCalendar ::
  forall t m.
  ( Reflex t,
    MonadHold t m,
    MonadFix m
  ) =>
  Dynamic t Graph ->
  Dynamic t (Map FilePath Pandoc) ->
  (Event t (Map FilePath (Maybe Pandoc))) ->
  m (Event t (Map FilePath (Maybe (IO ByteString))))
pluginCalendar _graphD _pandocD pandocE = do
  let diaryFilesE =
        ffilter (not . null) $
          Map.filterWithKey (\k _ -> isDiaryFileName k) <$> pandocE
  diaryFilesD <-
    fmap Map.keysSet
      <$> foldDyn patchMap mempty diaryFilesE
  pure $
    fforMaybe
      ( attach (current diaryFilesD) $
          tagPromptlyDyn diaryFilesD diaryFilesE
      )
      $ \(oldFs, fs) ->
        -- Update the entire calendar file only if a dairy file got added or
        -- removed.
        if oldFs == fs
          then Nothing
          else Just $
            one $
              ("@Calendar.html",) $
                Just $
                  fmap snd $
                    renderStatic $ do
                      el "title" $ text "Calendar"
                      forM_ fs $ \fp ->
                        el "li" $ elAttr "a" ("href" =: toText (ViewNote.mdToHtml fp)) $ text $ noteFileTitle fp
  where
    isDiaryFileName =
      T.isPrefixOf "20" . toText

pluginViewNotes ::
  forall t m.
  ( Reflex t,
    MonadHold t m
  ) =>
  Dynamic t Graph ->
  Dynamic t (Map FilePath Pandoc) ->
  (Event t (Map FilePath (Maybe Pandoc))) ->
  m (Event t (Map FilePath (Maybe (IO ByteString))))
pluginViewNotes graphD pandocD pandocE = do
  -- Like `pandocE` but includes other notes whose backlinks have changed as a
  -- result of the update in `pandocE`
  let pandocAllE :: Event t (Map FilePath (Maybe Pandoc)) =
        ffor
          -- `attach` gets us the old graph. The promptly version gets the new
          -- one (updated in this frame)
          ( attach (current graphD) $
              attachPromptlyDyn (zipDyn graphD pandocD) pandocE
          )
          $ \(oldGraph, ((graph, docMap), fps)) ->
            Map.unions $
              ffor (Map.toList fps) $ \(fp, doc) ->
                let -- Consider only edges that were removed, modified or added.
                    esDirty =
                      symmetricDifference
                        (AM.postSet fp graph)
                        (AM.postSet fp oldGraph)
                    esR =
                      fforMaybe (Set.toList esDirty) $ \fp' -> do
                        -- Add linked doc not already marked as changed.
                        guard $ Map.notMember fp' fps
                        doc' <- Map.lookup fp' docMap
                        pure (fp', Just doc')
                 in Map.insert fp doc $ Map.fromList esR
  pure $
    ffor (attachPromptlyDyn graphD pandocAllE) $ \(graph, xs) ->
      Map.fromList $
        ffor (Map.toList xs) $ \(fp, mdoc) -> do
          let htmlFile = ViewNote.mdToHtml fp
          case mdoc of
            Nothing -> (htmlFile, Nothing)
            Just doc -> (htmlFile, Just $ ViewNote.render graph fp doc)

patchMap :: Ord k => Map k (Maybe a) -> Map k a -> Map k a
patchMap diff xs =
  let (toAdd, toDel) = Map.mapEither (maybeToLeft ()) diff
   in Map.union toAdd xs `Map.difference` toDel

symmetricDifference :: Ord a => Set a -> Set a -> Set a
symmetricDifference x y =
  (x `Set.union` y) `Set.difference` (x `Set.intersection` y)