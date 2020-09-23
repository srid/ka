module Main where

import Main.Utf8 (withUtf8)

main :: IO ()
main =
  withUtf8 $ do
    putTextLn "Hello, Haskell!"
