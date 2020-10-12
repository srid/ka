module Ka.Plugin where

import Data.Dependent.Sum (DSum (..))
import Ka.Graph (Graph)
import qualified Ka.Graph as G
import qualified Ka.Plugin.Calendar as Calendar
import qualified Ka.Plugin.ViewNote as ViewNote
import Ka.Route (Route)
import Reflex
import Reflex.Dom.Pandoc (PandocBuilder)
import Text.Pandoc.Definition (Pandoc)

data Doc a where
  Doc_Pandoc :: Doc Pandoc
  Doc_Calendar :: Doc (Set G.Thing)

type D t = Dynamic t (Map G.Thing (DSum Doc Identity))

renderDoc :: PandocBuilder t m => Graph -> G.Thing -> DSum Doc Identity -> m (Event t Route)
renderDoc g th = \case
  Doc_Pandoc :=> Identity doc ->
    ViewNote.render g th doc
  Doc_Calendar :=> Identity days ->
    Calendar.render days