{-# LANGUAGE RecursiveDo #-}

module Ka.Sidebar.Listing (render) where

import Control.Monad.Fix (MonadFix)
import Data.Tagged
import qualified Data.Text as T
import Ka.Graph (ThingName (..))
import qualified Ka.Plugin.Calendar as Calendar
import Ka.Route (Route (..), dynRouteLink, renderRouteText)
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
render xs = do
  rec q <- searchInput $ () <$ routeChanged
      let results = zipDynWith (filter . includeInListing) q xs
      routeChanged <- renderListing results
  pure routeChanged

searchInput ::
  DomBuilder t m =>
  -- | Clear the input field when this event fires
  Event t () ->
  m (Dynamic t (Maybe SearchQuery))
searchInput clearInput = do
  divClass "item" $ do
    divClass "ui input" $ do
      fmap (fmap mkSearchQuery . value) $
        inputElement $
          def
            & initialAttributes .~ ("placeholder" =: "Press / to search")
            & inputElementConfig_setValue .~ ("" <$ clearInput)

renderListing ::
  ( DomBuilder t m,
    MonadHold t m,
    PostBuild t m,
    MonadFix m,
    Prerender js t m
  ) =>
  Dynamic t [ThingName] ->
  m (Event t Route)
renderListing res = do
  fmap (switch . current . fmap leftmost) $
    simpleList res $ \th -> do
      let rDyn = Route_Node <$> th
      dynRouteLink rDyn (constDyn $ "class" =: "gray active item") $ do
        dyn_ $ renderRouteText <$> rDyn

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