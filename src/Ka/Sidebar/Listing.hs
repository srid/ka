module Ka.Sidebar.Listing (render) where

import Control.Monad.Fix (MonadFix)
import Data.Tagged
import qualified Data.Text as T
import Ka.Graph (ThingName (..))
import qualified Ka.Plugin.Calendar as Calendar
import Ka.Route (Route (..), renderRouteText, routeLinkWithAttr)
import Reflex.Dom.Core

type SearchQuery = Tagged "SearchQuery" Text

mkSearchQuery :: Text -> Maybe SearchQuery
mkSearchQuery (T.strip -> s) =
  if T.null s
    then Nothing
    else Just $ Tagged s

render ::
  forall t m js.
  ( DomBuilder t m,
    MonadHold t m,
    PostBuild t m,
    MonadFix m,
    Prerender js t m
  ) =>
  Dynamic t [ThingName] ->
  m (Event t Route)
render ths = do
  mq <-
    divClass "item" $ do
      divClass "ui input" $ do
        fmap (fmap mkSearchQuery . value) $
          -- TODO: clear this field when output route event fires
          inputElement $
            def
              & initialAttributes .~ ("placeholder" =: "Press / to search")
  let res = zipDynWith (filter . includeInListing) mq ths
  fmap (switch . current . fmap leftmost) $
    simpleList res $ \th -> do
      let rDyn = Route_Node <$> th
      switchHold never <=< dyn $
        ffor rDyn $ \r -> do
          routeLinkWithAttr r (constDyn $ "class" =: "gray active item") $ do
            renderRouteText r

-- | Include the given thing in the listing?
includeInListing :: Maybe SearchQuery -> ThingName -> Bool
includeInListing mq th =
  case mq of
    Nothing ->
      -- If the user is not searching, we apply the default filters.
      -- If they are currently searching, however, we don't apply them (allowing
      -- them to search through the otherwise filtered out things.)
      Calendar.includeInSidebar th
    Just (untag -> q) ->
      q `caseInsensitiveIsInfixOf` unThingName th
  where
    caseInsensitiveIsInfixOf p s =
      T.toLower p `T.isInfixOf` T.toLower s