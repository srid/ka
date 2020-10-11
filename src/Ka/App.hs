{-# LANGUAGE RecursiveDo #-}

module Ka.App where

import Commonmark (defaultSyntaxSpec)
import qualified Commonmark.Extensions as CE
import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import Ka.Graph (Graph)
import qualified Ka.Graph as G
import Ka.Markdown (noteExtension, parseMarkdown, queryLinksWithContext)
import qualified Ka.Plugin.Calendar as Calendar
import qualified Ka.Plugin.ViewNote as ViewNote
import Ka.Plugin.WikiLink (wikiLinkSpec)
import Ka.Route (Route)
import Ka.Watch (directoryFilesContent)
import Reflex hiding (mapMaybe)
import Reflex.Dom.Pandoc (PandocBuilder)
import Text.Pandoc.Definition (Pandoc)

data App t m = App
  { _app_graph :: Dynamic t Graph,
    _app_pandoc :: Dynamic t (Map FilePath Pandoc),
    _app_render :: Dynamic t (Map FilePath (m (Event t Route)))
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
  m (App t m)
kaApp = do
  fileContentE <- directoryFilesContent "." noteExtension
  logDiffEvent fileContentE
  let pandocE :: Event t (Map FilePath (Maybe Pandoc)) =
        ffor fileContentE $
          Map.mapWithKey $ \fp -> fmap $ \s ->
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
        Map.map (fmap $ fmap swap . Map.toList . queryLinksWithContext)
  pandocD :: Dynamic t (Map FilePath Pandoc) <-
    foldDyn G.patchMap mempty pandocE
  -- NOTE: If two plugins produce the same file, the later plugin's output will
  -- be used, discarding the formers. That is what `flip Map.union` effectively does.
  renderE <-
    fmap (mergeWith $ flip Map.union) $
      sequence
        -- TODO: Eventually create a proper Plugin type to hold these functions.
        [ ViewNote.runPlugin graphD pandocD pandocE,
          Calendar.runPlugin graphD pandocD pandocE
        ]
  renderD <-
    foldDyn G.patchMap mempty renderE
  pure $ App graphD pandocD renderD

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
