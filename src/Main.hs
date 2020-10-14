module Main where

import qualified Ka.View as View
import Main.Utf8 (withUtf8)
import Reflex.Dom (mainWidgetWithHead)
import System.Directory (getCurrentDirectory)
import System.IO (BufferMode (LineBuffering), hSetBuffering)

main :: IO ()
main =
  withUtf8 $ do
    hSetBuffering stdout LineBuffering
    putStrLn . ("Notes dir " <>) =<< getCurrentDirectory
    mainWidgetWithHead
      View.headWidget
      View.bodyWidget
