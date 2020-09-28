module Ka.Plugin where

import qualified Commonmark as CM
import qualified Commonmark.Pandoc as CP
import qualified Data.Map.Strict as Map
import Ka.Diff (Changed (..), V (..))
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc)

type CMSyntaxSpec =
  CM.SyntaxSpec
    (Either CM.ParseError)
    (CP.Cm () B.Inlines)
    (CP.Cm () B.Blocks)

data Plugin = Plugin
  { -- | Custom extensions to apply when parsing Markdown
    commonmarkSpec :: CMSyntaxSpec,
    -- | Transform Pandoc type before graph creation
    docTransformer :: (Pandoc -> Pandoc),
    -- | Transform Pandoc type after graph creation
    docTransformerWithGraph :: (() -> Pandoc -> Pandoc),
    -- | Files to generate
    fileGenerator :: Map FilePath (V Pandoc) -> Map FilePath (Changed Text)
  }

demoPlugin :: Plugin
demoPlugin =
  Plugin
    { commonmarkSpec = mempty,
      docTransformer = id,
      docTransformerWithGraph = const id,
      fileGenerator = \docs ->
        Map.mapMaybe id $
          flip Map.mapWithKey docs $ \_k -> \case
            VChanged ch ->
              Just $
                flip fmap ch $ \_doc ->
                  "TODO: write this doc"
            VSame _ -> Nothing
    }