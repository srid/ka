module Ka.Markdown
  ( parseMarkdown,
    queryLinks,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Pandoc as CP
import Ka.Plugin (CMSyntaxSpec)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (..))

parseMarkdown :: CMSyntaxSpec -> FilePath -> Text -> Pandoc
parseMarkdown spec fp s =
  either (error . show) toPandoc $ join $ CM.commonmarkWith spec fp s
  where
    toPandoc = Pandoc mempty . B.toList . CP.unCm

queryLinks :: FilePath -> Pandoc -> Set FilePath
queryLinks _ _ = mempty -- TODO
