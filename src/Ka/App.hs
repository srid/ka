{-# LANGUAGE RecursiveDo #-}

module Ka.App where

import Commonmark (defaultSyntaxSpec)
import qualified Commonmark.Extensions as CE
import qualified Data.Map.Strict as Map
import Ka.Graph (Graph)
import qualified Ka.Graph as G
import Ka.Markdown (noteExtension, parseMarkdown, queryLinksWithContext)
import qualified Ka.Plugin.Calendar as Calendar
import qualified Ka.Plugin.ViewNote as ViewNote
import Ka.Plugin.WikiLink (wikiLinkSpec)
import Ka.Watch (directoryFilesContent)
import Reflex hiding (mapMaybe)
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
  fmap (mergeWith $ flip Map.union) $
    sequence
      -- TODO: Eventually create a proper Plugin type to hold these functions.
      [ ViewNote.runPlugin graphD pandocD pandocE,
        Calendar.runPlugin graphD pandocD pandocE
      ]
