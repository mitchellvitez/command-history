{-# LANGUAGE OverloadedStrings #-}

module CommandHistory
    ( CommandHistory
    , toPrevCommand
    , toNextCommand
    , setCurrCommand
    , startNewCommand
    , openCommandHistory
    ) where

import Control.Monad (when)
import Data.IORef
import Data.Maybe (isNothing)
import Data.Text (Text)
import System.Directory (renameFile)
import qualified Data.Text as T

data CommandHistory =
  CommandHistory
    { currCommand :: IORef Text
    , historyFile :: FilePath
    , counter :: IORef Int
    }

openCommandHistory :: FilePath -> IO CommandHistory
openCommandHistory path = do
  currCommandRef <- newIORef ""
  -- starts at -1 because we're "one away" from the last command entered
  counterRef <- newIORef (-1)
  return
    CommandHistory 
      { currCommand = currCommandRef
      , historyFile = path
      , counter = counterRef
      }

toPrevCommand :: CommandHistory -> IO (Maybe Text)
toPrevCommand ch =
  changeCommand ch 1

toNextCommand :: CommandHistory -> IO (Maybe Text)
toNextCommand ch =
  changeCommand ch (-1)

changeCommand :: CommandHistory -> Int -> IO (Maybe Text)
changeCommand ch counterChange = do
  modifyIORef (counter ch) (+counterChange)
  prefix <- readIORef $ currCommand ch
  count <- readIORef $ counter ch
  file <- readFile $ historyFile ch
  let result = maybeIndex count . filter (prefix `T.isPrefixOf`) . map T.pack . lines $ file
  when (isNothing result) $ modifyIORef (counter ch) (subtract counterChange)
  return result

maybeIndex :: Int -> [a] -> Maybe a
maybeIndex _ [] = Nothing
maybeIndex 0 (x:_) = Just x
maybeIndex n (x:xs) = maybeIndex (n-1) xs

setCurrCommand :: CommandHistory -> Text -> IO ()
setCurrCommand ch cmd = do
  writeIORef (counter ch) (-1)
  writeIORef (currCommand ch) cmd

startNewCommand :: CommandHistory -> IO ()
startNewCommand ch = do
  writeIORef (counter ch) (-1)
  command <- readIORef $ currCommand ch
  writeIORef (currCommand ch) ""
  -- write command to first line of history file
  file <- readFile $ historyFile ch
  let temp = historyFile ch ++ ".tmp"
  writeFile temp $ T.unpack command ++ "\n" ++ file
  renameFile temp $ historyFile ch

