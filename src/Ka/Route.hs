module Ka.Route where

import Reflex.Dom

data Route
  = Route_Main
  | Route_Node FilePath
  deriving (Eq, Show)

routeLink :: DomBuilder t m => Route -> m () -> m (Event t Route)
routeLink r w = do
  e <- clickEvent $ elClass' "a" "route" w
  pure $ r <$ e

-- | Get the click event on an element
--
-- Use as:
--   clickEvent $ el' "a" ...
clickEvent ::
  ( DomBuilder t m,
    HasDomEvent t target 'ClickTag
  ) =>
  m (target, a) ->
  m (Event t ())
clickEvent w =
  fmap (fmap (const ()) . domEvent Click . fst) w
