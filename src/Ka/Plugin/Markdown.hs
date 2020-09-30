module Ka.Plugin.Markdown
  ( markdownPlugin,
  )
where

import qualified Commonmark as CM
import Data.Default (Default (def))
import Ka.Plugin

markdownPlugin :: Plugin
markdownPlugin =
  def
    { commonmarkSpec = CM.defaultSyntaxSpec
    }
