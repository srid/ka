{-# LANGUAGE RecursiveDo #-}

module Ka.Plugin.Calendar
  ( runPlugin,
    render,
    style,
    thingPanel,
    includeInSidebar,
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import Clay (Css, (?))
import qualified Clay as C
import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (parseTimeM)
import Data.Time.Calendar (Day, addDays, toGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Ka.Graph (Graph, ThingName (..))
import qualified Ka.Graph as G
import Ka.Route (Route (..), routeLink)
import Reflex.Dom.Core hiding (Link)
import Relude.Extra (groupBy)
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
  m (Event t (Map ThingName (Maybe (Set Day))))
runPlugin _graphD _pandocD pandocE = do
  let diaryFilesE =
        ffilter (not . null) $
          Map.mapMaybeWithKey (\k mv -> ffor mv $ \_ -> parseDairyThing k) <$> pandocE
  diaryFilesD <-
    fmap (Set.fromList . Map.elems)
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
  forall js t m.
  ( Prerender js t m,
    PostBuild t m,
    DomBuilder t m,
    MonadHold t m,
    MonadFix m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO (Performable m)
  ) =>
  Dynamic t Graph ->
  Dynamic t (Set Day) ->
  m (Event t Route)
render g (fmap Set.toList -> days) = do
  linksTo <- noteTitleInput g
  -- Tags days with a Bool to indicate if they should be highlighted. Entries
  -- linking to `linksTo` will be highlighted. This is an experimental feature.
  let daysTagged = ffor3 days linksTo g $ \days' mth graph ->
        ffor days' $ \day ->
          let t = maybe False (`Set.member` AM.postSet (dayThing day) graph) mth
           in (day, t)
  divClass "ui basic calendar segment" $ do
    let grouped :: Dynamic t (Map (Integer, Int) (NonEmpty (Day, Bool))) =
          groupBy ((\(y, m, _) -> (y, m)) . toGregorian . fst) . reverse <$> daysTagged
    divClass "ui message" $ do
      el "p" $ text "This is not a real calendar layout."
    fmap (switch . current . fmap leftmost) $
      simpleList (Map.toList <$> grouped) $ \monthDyn -> do
        elClass "h2" "header" $
          dynText $ show . fst <$> monthDyn
        divClass "ui seven column right aligned padded grid" $ do
          fmap (switch . current . fmap leftmost) $
            simpleList (toList . snd <$> monthDyn) $ \dayD ->
              elAttr "a" ("class" =: "column") $
                switchHold never <=< dyn $
                  ffor dayD $ \(day, highlight) -> do
                    let r = Route_Node $ dayThing day
                        (_y, _m, d) = toGregorian day
                    elClass "span" (bool "" "highlight" highlight) $
                      routeLink r $ text $ show d

style :: Css
style = do
  ".calendar" ? do
    "span.highlight" ? do
      C.backgroundColor C.yellow

noteTitleInput ::
  forall t m.
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO (Performable m)
  ) =>
  Dynamic t Graph ->
  m (Dynamic t (Maybe ThingName))
noteTitleInput g = do
  rec val <-
        elDynAttr "div" inputClasses $ do
          fmap value $
            inputElement $
              def
                & initialAttributes .~ ("placeholder" =: "Enter a note title")
      mthing <-
        flip debounceWith (zipDyn g val) $ \(graph, x) ->
          let th = ThingName x
           in if AM.hasVertex th graph
                then Just th
                else Nothing
      -- Display error on non-empty input if it is not in graph
      let inputClasses = ffor (zipDyn val mthing) $ \case
            ("", _) -> ("class" =: "ui input")
            (_, Nothing) -> ("class" =: "ui error input")
            _ -> ("class" =: "ui input")
  pure mthing
  where
    debounceWith :: (a -> Maybe b) -> Dynamic t a -> m (Dynamic t (Maybe b))
    debounceWith f =
      holdDyn Nothing <=< fmap (fmap f) . debounce 0.5 . updated

calThing :: ThingName
calThing =
  ThingName "+Calendar"

thingPanel ::
  ( DomBuilder t m,
    PostBuild t m,
    MonadFix m,
    MonadHold t m,
    Prerender js t m
  ) =>
  Dynamic t Graph ->
  Dynamic t ThingName ->
  m (Event t Route)
thingPanel _g thDyn = do
  thDynM <- maybeDyn $ parseDairyThing <$> thDyn
  switchHold never <=< dyn $
    ffor thDynM $ \case
      Nothing ->
        pure never
      Just dayDyn -> do
        divClass "ui calendar small basic segment three column grid" $ do
          switchHold never <=< dyn $
            ffor dayDyn $ \day -> do
              let prev = addDays (-1) day
                  next = addDays 1 day
                  prevR = Route_Node . ThingName . show $ prev
                  nextR = Route_Node . ThingName . show $ next
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

includeInSidebar :: ThingName -> Bool
includeInSidebar =
  isNothing . parseDairyThing

parseDairyThing :: ThingName -> Maybe Day
parseDairyThing th =
  parseTimeM True defaultTimeLocale "%Y-%m-%d" (toString $ unThingName th)

dayThing :: Day -> ThingName
dayThing d =
  ThingName . toText $ formatTime defaultTimeLocale "%Y-%m-%d" d