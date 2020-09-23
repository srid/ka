module Main where

import Main.Utf8 (withUtf8)
import System.FilePattern.Directory (getDirectoryFiles)

main :: IO ()
main =
  withUtf8 $ do
    notes <- getDirectoryFiles "." ["*.md"]
    print notes
