module Main where

import qualified Data.Text as T
import qualified Ka.View as View
import Main.Utf8 (withUtf8)
import Reflex.Dom (mainWidgetWithHead)
import System.Directory (getCurrentDirectory, getHomeDirectory, withCurrentDirectory)
import System.Environment (getArgs)
import System.IO (BufferMode (LineBuffering), hSetBuffering)

main :: IO ()
main =
  withUtf8 $ do
    customDir <- getArg0
    withCurrentDirectoryMaybe customDir $ do
      notesDir <- getCurrentDirectory
      notebookId <- pathAsShortTitle notesDir
      hSetBuffering stdout LineBuffering
      putStrLn $ "Notes dir " <> notesDir
      mainWidgetWithHead
        (View.headWidget notebookId)
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

pathAsShortTitle :: FilePath -> IO Text
pathAsShortTitle (toText -> fp) = do
  homeDir <- toText <$> getHomeDirectory
  pure $ T.replace homeDir "~" fp
