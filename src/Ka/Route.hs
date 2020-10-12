module Ka.Route where

import Ka.Graph (Thing)
import Reflex.Dom

data Route
  = Route_Main
  | Route_Node Thing
  deriving (Eq, Show)

-- TODO: This should scroll to top after route switch
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
