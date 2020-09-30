module Ka.Plugin where

import qualified Commonmark as CM
import qualified Commonmark.Inlines as CM
import qualified Commonmark.Pandoc as CP
import Commonmark.TokParsers (noneOfToks, symbol)
import Commonmark.Tokens (TokType (LineEnd, Symbol))
import Data.Default (Default)
import qualified Data.Map.Strict as Map
import Ka.Diff (Changed (..), V (..))
import Reflex.Dom.Core
import Reflex.Dom.Pandoc.Document
import System.FilePath ((-<.>))
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc)
import qualified Text.Parsec as P

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
    fileGenerator :: Map FilePath (V Pandoc) -> Map FilePath (Changed (IO ByteString))
  }

instance Default Plugin where
  def =
    Plugin mempty id (const id) (const mempty)

demoPlugin :: Plugin
demoPlugin =
  def
    { commonmarkSpec =
        mconcat
          [ CM.defaultSyntaxSpec,
            wikiLinkSpec (<> ".html")
          ],
      fileGenerator = \docs ->
        Map.fromList $
          catMaybes $
            flip fmap (Map.toList docs) $ \(k, v) -> case v of
              VChanged ch ->
                Just $
                  (k -<.> ".html",) $
                    flip fmap ch $ \doc ->
                      renderReflexWidget $ noteWidget doc
              VSame _ ->
                Nothing
    }

-- Wiki link plugin

wikiLinkSpec ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il) =>
  -- | Transform the wikilink inner text to a working URL
  --
  -- Eg: @(<> ".html")
  (Text -> Text) ->
  CM.SyntaxSpec m il bl
wikiLinkSpec f =
  mempty
    { CM.syntaxInlineParsers = [pLink]
    }
  where
    pLink :: (Monad m, CM.IsInline il) => CM.InlineParser m il
    pLink = P.try $ do
      s <- wikiLinkP
      let url = f s
          title = ""
      pure $ CM.link url title (CM.str s)
    wikiLinkP :: Monad m => P.ParsecT [CM.Tok] s m Text
    wikiLinkP = do
      let allowedChars = noneOfToks [Symbol ']', LineEnd]
      void $ P.count 2 $ symbol '['
      s <- fmap CM.untokenize $ some allowedChars
      void $ P.count 2 $ symbol ']'
      pure s

-- Render plugin

renderReflexWidget :: forall x. StaticWidget x () -> IO ByteString
renderReflexWidget w =
  fmap snd $ renderStatic w

noteWidget :: PandocBuilder t m => Pandoc -> m ()
noteWidget doc =
  elPandoc defaultConfig doc