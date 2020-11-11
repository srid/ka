module Ka.ScopeView where

import Control.Monad.Fix (MonadFix)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Ka.Graph (ThingName, unThingName)
import Ka.Route
import Ka.Scope (ThingScope, isSubScope, splitScope)
import Reflex.Dom

renderScopeBreadcrumb ::
  forall js t m.
  (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m, Prerender js t m) =>
  Dynamic t (R Route) ->
  Dynamic t (Map ThingName ThingScope) ->
  m (Event t (R Route))
renderScopeBreadcrumb r scopesDyn = do
  mScopeSplit <- maybeDyn $
    ffor2 scopesDyn r $ \scopes -> \case
      Route_Scope :/ sc ->
        pure $ splitScope sc
      Route_Node :/ th -> do
        scope <- Map.lookup th scopes
        pure (Just scope, toString $ unThingName th)
  switchHold never <=< dyn $
    ffor mScopeSplit $ \case
      Nothing ->
        -- Thing 404
        pure never
      Just (split :: Dynamic t (Maybe ThingScope, FilePath)) -> do
        a <- holdUniqDyn $ fst <$> split
        b <- holdUniqDyn $ toText . snd <$> split
        renderScopeCrumbs a b
  where
    renderScopeCrumbs ::
      Dynamic t (Maybe ThingScope) ->
      Dynamic t Text ->
      m (Event t (R Route))
    renderScopeCrumbs mscope currTitle = do
      divClass "ui top attached segment breadcrumb" $ do
        mscope' <- maybeDyn mscope
        e <- switchHold never <=< dyn $
          ffor mscope' $ \case
            Nothing -> pure never
            Just scope -> do
              let xs = inits <$> scope
              fmap (switch . current . fmap leftmost) $ do
                simpleList xs $ \scopeDyn -> do
                  e <- dynRouteLink ((Route_Scope :/) <$> scopeDyn) (constDyn $ "class" =: "section") $ do
                    let node = maybe "/" head . nonEmpty . reverse <$> scopeDyn
                    dynText $ toText <$> node
                  elClass "i" "right angle icon divider" blank
                  pure e
        divClass "active section" $ dynText currTitle
        pure e

renderScopeContents ::
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    Prerender js t m
  ) =>
  Dynamic t (Map ThingName ThingScope) ->
  Dynamic t ThingScope ->
  m (Event t (R Route))
renderScopeContents thingScopes scopeDyn = do
  divClass "ui attached basic segment" $ do
    let thingsInScope = ffor2 scopeDyn thingScopes $ \currScope scopes ->
          Map.keys $
            Map.filter (== currScope) scopes
        subScopes = ffor2 scopeDyn thingScopes $ \currScope (nub . Map.elems -> scopes) ->
          scopes
            & filter (`isSubScope` currScope)
    evt <- divClass "ui relaxed list scope-contents" $ do
      e1 <- fmap (switch . current . fmap leftmost) $
        simpleList subScopes $ \sc -> do
          let rDyn = (Route_Scope :/) <$> sc
          folderName <- holdUniqDyn $ toText . snd . splitScope <$> sc
          folderItem $
            dynRouteLink rDyn (constDyn mempty) $ do
              el "b" $ dynText folderName
      e2 <- fmap (switch . current . fmap leftmost) $
        simpleList thingsInScope $ \th -> do
          let rDyn = (Route_Node :/) <$> th
          fileItem $
            dynRouteLink rDyn (constDyn mempty) $ do
              dyn_ $ renderRouteText <$> rDyn
      pure $ leftmost [e1, e2]
    divClass "ui mini horizontal statistic" $ do
      divClass "value" $ do
        dynText $ show . Map.size <$> thingScopes
      divClass "label" $ do
        text "Notes"
    pure evt
  where
    folderItem w = do
      divClass "scope item" $ do
        scopeIcon
        divClass "content" $ do
          divClass "header" w
    fileItem w = do
      divClass "thing item" $ do
        thingIcon
        divClass "content" $ do
          divClass "description" w

scopeIcon :: DomBuilder t m => m ()
scopeIcon =
  elClass "i" "folder icon" blank

thingIcon :: DomBuilder t m => m ()
thingIcon =
  elClass "i" "sticky note outline icon" blank