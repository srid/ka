module Ka.Plugin.Calendar
  ( runPlugin,
    render,
  )
where

import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Ka.Graph (Graph, ThingName (..))
import qualified Ka.Graph as G
import Ka.Route (Route, renderThingLink)
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
  Dynamic t (Map ThingName Pandoc) ->
  (Event t (Map ThingName (Maybe Pandoc))) ->
  m (Event t (Map ThingName (Maybe (Set ThingName))))
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
                (ThingName "0-Calendar",) $
                  Just fs
  where
    isDiaryFileName =
      T.isPrefixOf "20" . unThingName

render :: (Prerender js t m, DomBuilder t m) => Set ThingName -> m (Event t Route)
render (Set.toList -> fs) = do
  divClass "ui divided equal width compact seven column grid" $ do
    fmap leftmost $
      forM fs $ \fp ->
        elAttr "a" ("class" =: "column") $
          renderThingLink fp
