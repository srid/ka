module Main where

import qualified Ka.View as View
import Main.Utf8 (withUtf8)
import Reflex.Dom
import System.Directory (withCurrentDirectory)
import System.IO (BufferMode (LineBuffering), hSetBuffering)

notesDir :: FilePath
notesDir = "/home/srid/Sync/zk"

main :: IO ()
main =
  withUtf8 $
    withCurrentDirectory notesDir $ do
      hSetBuffering stdout LineBuffering
      mainWidgetWithHead
        View.headWidget
        View.bodyWidget
