module Ka.Plugin.Calendar
  ( runPlugin,
  )
where

import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Ka.Graph (Graph)
import qualified Ka.Graph as G
import Ka.Route (Route)
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
  Dynamic t (Map G.Thing Pandoc) ->
  (Event t (Map G.Thing (Maybe Pandoc))) ->
  m (Event t (Map G.Thing (Maybe (m (Event t Route)))))
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
                (G.Thing "Calendar",) $
                  Just $ calWidget fs >> pure never
  where
    calWidget fs = do
      -- V.kaTemplate mempty (text "Calendar") $ do
      elClass "h1" "header" $ text "Calendar"
      divClass "ui divided equal width compact seven column grid" $ do
        forM_ fs $ \fp ->
          elAttr "a" ("class" =: "column" <> "href" =: G.unThing fp) $
            text $ G.unThing fp
    isDiaryFileName =
      T.isPrefixOf "20" . G.unThing
