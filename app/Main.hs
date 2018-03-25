{-# LANGUAGE OverloadedStrings #-}
module Main where

import CommandHistory
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import UI.NCurses
import qualified Data.Text as T

main :: IO ()
main = do
  history <- openCommandHistory "commands.txt"
  runCurses $ do
    setEcho False
    window <- defaultWindow
    setup window
    enterCommands window history ""

enterCommands :: Window -> CommandHistory -> Text -> Curses ()
enterCommands window history command = do
  render
  event <- getEvent window Nothing
  let loop = enterCommands window history
  case event of
    Just (EventCharacter '\ESC') ->
      return ()

    Just (EventSpecialKey KeyUpArrow) -> do
      cmd <- liftIO $ toPrevCommand history
      setCommand window history command cmd

    Just (EventSpecialKey KeyDownArrow) -> do
      cmd <- liftIO $ toNextCommand history
      setCommand window history command cmd

    Just (EventCharacter '\DEL') ->
      if T.null command then loop command else do
        let newCommand = T.init command
        reset window
        updateWindow window $ drawText newCommand 
        liftIO $ setCurrCommand history newCommand
        loop newCommand

    Just (EventCharacter '\n') -> do
      unless (T.null command) $ do
        liftIO $ setCurrCommand history command
        liftIO $ startNewCommand history
      reset window
      loop ""

    Just (EventCharacter c) -> do
      let newCommand = T.snoc command c
      updateWindow window . drawText $ T.singleton c
      liftIO $ setCurrCommand history newCommand
      loop newCommand

    _ -> loop command

setCommand :: Window -> CommandHistory -> Text -> Maybe Text -> Curses ()
setCommand window history command cmd =
  case cmd of
    Nothing -> enterCommands window history command
    Just t -> do
      reset window
      updateWindow window $ drawText t
      enterCommands window history t

reset :: Window -> Curses ()
reset window =
  updateWindow window $ do
    moveCursor 5 2
    clearLine

setup :: Window -> Curses ()
setup window = do
    white <- newColorID ColorWhite ColorDefault 1
    blue <- newColorID ColorBlue ColorDefault 2
    green <- newColorID ColorGreen ColorDefault 3
    yellow <- newColorID ColorYellow ColorDefault 4
    updateWindow window $ do
      setColor green
      moveCursor 0 0
      drawText "* Command History Manager * "

      setColor blue
      moveCursor 1 0
      drawText "- Enter commands below"
      moveCursor 2 0
      drawText "- Page through with up/down arrow"
      moveCursor 3 0
      drawText "- Paging autocompletes based on command prefix"
      moveCursor 4 0
      drawText "- Press Esc to quit"

      setColor yellow
      moveCursor 5 0
      drawText "> "

      setColor white
    reset window
