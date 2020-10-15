module Main where

import qualified Ka.View as View
import Main.Utf8 (withUtf8)
import Reflex.Dom (mainWidgetWithHead)
import System.Directory (getCurrentDirectory, withCurrentDirectory)
import System.Environment (getArgs)
import System.IO (BufferMode (LineBuffering), hSetBuffering)

main :: IO ()
main =
  withUtf8 $ do
    customDir <- getArg0
    withCurrentDirectoryMaybe customDir $ do
      hSetBuffering stdout LineBuffering
      putStrLn . ("Notes dir " <>) =<< getCurrentDirectory
      mainWidgetWithHead
        View.headWidget
        View.bodyWidget

-- | Get the first CLI argument
getArg0 :: IO (Maybe FilePath)
getArg0 =
  getArgs >>= \case
    path : _ -> pure $ Just path
    [] -> pure Nothing

-- | Like withCurrentDirectory but optional
withCurrentDirectoryMaybe :: Maybe FilePath -> IO a -> IO a
withCurrentDirectoryMaybe mf action =
  case mf of
    Nothing -> action
    Just f -> withCurrentDirectory f action