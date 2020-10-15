module Ka.Plugin.Calendar
  ( runPlugin,
    render,
    thingPanel,
  )
where

import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (parseTimeM)
import Data.Time.Calendar (Day, addDays)
import Data.Time.Format (defaultTimeLocale)
import Ka.Graph (Graph, ThingName (..))
import qualified Ka.Graph as G
import Ka.Route (Route (..), renderThingLink, routeLink)
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
          Map.filterWithKey (\k _ -> isJust $ parseDairyThing k) <$> pandocE
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
                (calThing, Just fs)

render ::
  ( Prerender js t m,
    PostBuild t m,
    DomBuilder t m,
    MonadHold t m,
    MonadFix m
  ) =>
  Dynamic t (Set ThingName) ->
  m (Event t Route)
render (fmap Set.toList -> fs) = do
  divClass "ui divided equal width compact seven column grid" $ do
    fmap (switch . current . fmap leftmost) $
      simpleList fs $ \fp ->
        elAttr "a" ("class" =: "column") $
          switchHold never <=< dyn $ ffor fp $ renderThingLink

calThing :: ThingName
calThing =
  ThingName "0-Calendar"

thingPanel ::
  ( DomBuilder t m,
    PostBuild t m,
    Prerender js t m
  ) =>
  Dynamic t Graph ->
  ThingName ->
  m (Event t Route)
thingPanel _g th = do
  case parseDairyThing th of
    Nothing ->
      pure never
    Just day -> do
      let prev = addDays (-1) day
          next = addDays 1 day
          prevR = Route_Node . ThingName . show $ prev
          nextR = Route_Node . ThingName . show $ next
      divClass "ui calendar small basic segment three column grid" $ do
        -- TODO: Show these only if they exist in the graph
        e1 <-
          divClass "column" $
            routeLink prevR $ text $ show prev
        ec <-
          divClass "center aligned column" $
            routeLink (Route_Node calThing) $ text "Calendar"
        e2 <-
          divClass "right aligned column" $
            routeLink nextR $ text $ show next
        pure $ leftmost [e1, ec, e2]

parseDairyThing :: ThingName -> Maybe Day
parseDairyThing th =
  parseTimeM True defaultTimeLocale "%Y-%-m-%-d" (toString $ unThingName th)
