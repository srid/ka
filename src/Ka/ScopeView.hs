module Ka.ScopeView where

import Control.Monad.Fix (MonadFix)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Ka.Graph (ThingName)
import Ka.Route
import Ka.Scope (ThingScope, splitScope)
import Reflex.Dom

renderScopeCrumbs ::
  (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender js t m) =>
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
renderScopeContents docs scopeDyn = do
  split <- holdUniqDyn $ splitScope <$> scopeDyn
  -- TODO consolidate with Thing.hs's call
  e0 <- renderScopeCrumbs (fst <$> split) (toText . snd <$> split)
  e1 <- divClass "ui attached basic segment" $ do
    let scopeDocs = ffor2 scopeDyn docs $ \currScope scopes ->
          Map.keys $
            flip Map.filter scopes $
              \scope ->
                scope == currScope
        subScopes = ffor2 scopeDyn docs $ \currScope scopes ->
          nub $
            Map.elems $
              flip Map.filter scopes $
                \scope ->
                  if null currScope
                    then length scope == 1
                    else length scope == length currScope + 1 && currScope `isPrefixOf` scope
    evt <- fmap (switch . current . fmap leftmost) $
      divClass "ui list" $ do
        -- TODO: Is the scope data-type perfect?
        -- Consider the case of multiple notebooks passed as arguments to CLI
        -- Duplicate daily notes: we should allow them! and yet resolve it correctly.
        e1 <- simpleList subScopes $ \sc -> do
          let rDyn = (Route_Scope :/) <$> sc
          folderName <- holdUniqDyn $ toText . snd . splitScope <$> sc
          dynRouteLink rDyn (constDyn mempty) $ do
            el "li" $ el "b" $ dynText folderName
        e2 <- simpleList scopeDocs $ \th -> do
          let rDyn = (Route_Node :/) <$> th
          dynRouteLink rDyn (constDyn $ "class" =: "scope-item") $ do
            el "li" $ dyn_ $ renderRouteText <$> rDyn
        pure $ zipDynWith (<>) e1 e2
    el "hr" blank
    let cnt = Map.size <$> docs
    el "p" $ do
      text "Note count: "
      dynText $ show <$> cnt
    pure evt
  pure $ leftmost [e0, e1]
