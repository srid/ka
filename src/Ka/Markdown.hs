module Ka.Markdown
  ( parseMarkdown,
    queryLinks,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Pandoc as CP
import qualified Data.Set as Set
import qualified Data.Text as T
import Ka.Plugin (CMSyntaxSpec)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Inline (Link), Pandoc (..))
import qualified Text.Pandoc.Walk as W

parseMarkdown :: CMSyntaxSpec -> FilePath -> Text -> Pandoc
parseMarkdown spec fp s =
  either (error . show) toPandoc $ join $ CM.commonmarkWith spec fp s
  where
    toPandoc = Pandoc mempty . B.toList . CP.unCm

queryLinks :: Pandoc -> Set FilePath
queryLinks = Set.fromList . W.query go
  where
    go :: Inline -> [FilePath]
    go = maybeToList . filePathFromInline
    filePathFromInline :: Inline -> Maybe FilePath
    filePathFromInline = \case
      Link _attr _inlines (url, _title) -> do
        guard $ not $ "/" `T.isInfixOf` url
        guard $ ".md" `T.isSuffixOf` url
        pure $ toString url
      _ ->
        Nothing
