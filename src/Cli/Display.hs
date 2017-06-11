{-# LANGUAGE OverloadedStrings #-}

module Cli.Display where

import           Control.Monad (forM_)
import           Data.Text
import qualified Data.Vector as V
import           UI.NCurses

run :: IO ()
run = runCurses app

newtype ItemID = ItemID Text

data DisplayState = DisplayState
  { selectedItem :: Int
  , display :: DisplayElement
  }

data DisplayElement
 = Title State Text
 | CollapsableList DisplayElement [DisplayElement]


data State = None | Selected

title :: Text -> DisplayElement
title name = Title None name

selected :: DisplayElement -> DisplayElement
selected (Title _ name) = Title Selected name
selected (CollapsableList title items) = CollapsableList (selected title) items

list :: Text -> [DisplayElement] -> DisplayElement
list name items = CollapsableList (title name) items

testApp :: DisplayElement
testApp = list "title" [title "one", selected $ list "two" [title "wow", title "cool"], title "three"]

app :: Curses ()
app = do
  window <- newWindow 80 80 0 0
  magenta <- newColorID ColorRed ColorCyan 1
  white <- newColorID ColorWhite ColorDefault 2

  updateWindow window (drawApp (DrawState white magenta) (DisplayState 1 testApp))
  getEvent window Nothing
  closeWindow window

drawApp :: DrawState -> DisplayState -> Update ()
drawApp s app = drawDisplayElement s doDraw app
  where doDraw = drawDisplayElement s doDraw

data DrawState = DrawState
  { normalColor :: ColorID
  , highlightColor :: ColorID
  }

type DrawFn = DisplayState -> Update ()

drawDisplayElement :: DrawState -> DrawFn -> DisplayState -> Update ()
drawDisplayElement s fn ds = do
  case (display ds) of
    Title None t -> drawText t
    Title Selected t -> do
      setColor $ highlightColor s
      drawText t
      setColor $ normalColor s
    CollapsableList title items -> do
      (_, col) <- cursorPosition
      fn (DisplayState ((selectedItem ds) - 1) title)
      forM_ items (displayItem col)
        where displayItem origCol item = do
                (row, _) <- cursorPosition
                moveCursor (row + 1) origCol
                drawText "  * "
                fn (DisplayState ((selectedItem ds) - 1) item)
