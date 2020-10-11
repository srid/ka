module Ka.Plugin.Calendar
  ( runPlugin,
  )
where

import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Ka.Graph (Graph)
import qualified Ka.Graph as G
import Ka.Markdown (noteFileTitle)
import qualified Ka.View as V
import Reflex.Dom.Core hiding (Link)
import Text.Pandoc.Definition (Pandoc)

runPlugin ::
  forall t m.
  ( Reflex t,
    MonadHold t m,
    MonadFix m,
    DomBuilder t m
  ) =>
  Dynamic t Graph ->
  Dynamic t (Map FilePath Pandoc) ->
  (Event t (Map FilePath (Maybe Pandoc))) ->
  m (Event t (Map FilePath (Maybe (m ()))))
runPlugin _graphD _pandocD pandocE = do
  let diaryFilesE =
        ffilter (not . null) $
          Map.filterWithKey (\k _ -> isDiaryFileName k) <$> pandocE
  diaryFilesD <-
    fmap Map.keysSet
      <$> foldDyn G.patchMap mempty diaryFilesE
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
          else
            Just $
              one $
                ("@Calendar.html",) $
                  Just $ calWidget fs
  where
    calWidget fs = do
      V.kaTemplate mempty (text "Calendar") $ do
        elClass "h1" "header" $ text "Calendar"
        divClass "ui divided equal width compact seven column grid" $ do
          forM_ fs $ \fp ->
            elAttr "a" ("class" =: "column" <> "href" =: toText (V.mdToHtml fp)) $
              text $ noteFileTitle fp
    isDiaryFileName =
      T.isPrefixOf "20" . toText
