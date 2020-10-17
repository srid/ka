module Ka.Markdown
  ( noteExtension,
    mdFileThing,
    parseMarkdown,
    queryLinksWithContext,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Pandoc as CP
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Ka.Graph (ThingName (..))
import System.FilePath (dropExtension)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Attr, Block, Inline (Link), Pandoc (..), Target)
import qualified Text.Pandoc.Walk as W

type CMSyntaxSpec =
  CM.SyntaxSpec
    (Either CM.ParseError)
    (CP.Cm () B.Inlines)
    (CP.Cm () B.Blocks)

noteExtension :: String
noteExtension = ".md"

mdFileThing :: FilePath -> ThingName
mdFileThing = ThingName . toText . dropExtension

parseMarkdown :: CMSyntaxSpec -> FilePath -> Text -> Pandoc
parseMarkdown spec fp s =
  either (error . show) toPandoc $ join $ CM.commonmarkWith spec fp s
  where
    toPandoc = Pandoc mempty . B.toList . CP.unCm

queryLinksWithContext :: Pandoc -> Map FilePath [Block]
queryLinksWithContext doc =
  fmap nub $ Map.fromListWith (<>) . fmap (second one) $ W.query go doc
  where
    go :: Block -> [(FilePath, Block)]
    go blk =
      fmap (,blk) $ case blk of
        B.Para is ->
          W.query linksFromInline is
        B.Plain is ->
          W.query linksFromInline is
        B.LineBlock is ->
          W.query linksFromInline is
        B.Header _ _ is ->
          W.query linksFromInline is
        B.DefinitionList xs ->
          -- Gather all filenames linked, and have them put (see above) in the
          -- same definition list block.
          fmap concat $
            forM xs $ \(is, bss) ->
              let def = W.query linksFromInline is
                  body = fmap (fmap (fmap fst . go)) bss
               in def <> concat (concat body)
        _ -> mempty

    linksFromInline :: Inline -> [FilePath]
    linksFromInline = maybeToList . getNoteLinkFilePath

    getNoteLinkFilePath :: Inline -> Maybe FilePath
    getNoteLinkFilePath x = do
      (_attr, _inlines, (url, _title)) <- getNoteLink x
      pure $ toString url

    getNoteLink :: Inline -> Maybe (Attr, [Inline], Target)
    getNoteLink = \case
      Link attr inlines target@(url, _title) -> do
        guard $ not $ "/" `T.isInfixOf` url
        guard $ toText noteExtension `T.isSuffixOf` url
        pure (attr, inlines, target)
      _ ->
        Nothing
