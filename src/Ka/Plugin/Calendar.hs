module Ka.Plugin.Calendar
  ( runPlugin,
    render,
    thingPanel,
  )
where

import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
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
                (calThing, Just fs)

isDiaryFileName :: ThingName -> Bool
isDiaryFileName =
  T.isPrefixOf "20" . unThingName

render ::
  ( Prerender js t m,
    PostBuild t m,
    DomBuilder t m
  ) =>
  Set ThingName ->
  m (Event t Route)
render (Set.toList -> fs) = do
  divClass "ui divided equal width compact seven column grid" $ do
    fmap leftmost $
      forM fs $ \fp ->
        elAttr "a" ("class" =: "column") $
          renderThingLink fp

calThing :: ThingName
calThing =
  ThingName "0-Calendar"

thingPanel ::
  ( DomBuilder t m,
    PostBuild t m,
    Prerender js t m
  ) =>
  Graph ->
  ThingName ->
  m (Event t Route)
thingPanel _g th = do
  let mdate :: Maybe Day = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" (toString $ unThingName th)
  case mdate of
    Nothing ->
      pure never
    Just day -> do
      let prev = addDays (-1) day
          next = addDays 1 day
          prevR = Route_Node . ThingName . show $ prev
          nextR = Route_Node . ThingName . show $ next
      divClass "ui calendar small basic segment three column grid" $ do
        -- TODO: Show these only if they exist in the graph (but first make the
        -- graph a dynamic)
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
