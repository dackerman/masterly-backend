{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Cli.Display where

import           Control.Concurrent (forkIO)
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Text (Text, concat)
import qualified Graphics.Vty as V
import           Lens.Micro ((^.))

import qualified Brick.AttrMap as A
import qualified Brick.BChan as BC
import qualified Brick.Main as M
import           Brick.Types
  ( Widget
  )
import qualified Brick.Types as T
import           Brick.Util (fg, bg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import           Brick.Widgets.Core
  ( (<+>)
  , str
  , txt
  , vLimit
  , hLimit
  , vBox
  , hBox
  , withAttr
  , padTop
  , padRight
  , padBottom
  )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Data.Text.Zipper as Z
import qualified Data.Vector as Vec

import           Prelude hiding (concat)

drawUI :: AppState a -> [Widget Names]
drawUI AS{..} = [ui]
    where
      l = _asList
      label = str "Item " <+> cur <+> str " of " <+> total
      cur = case l^.(L.listSelectedL) of
        Nothing -> str "-"
        Just i -> str (show (i + 1))
      total = str $ show $ Vec.length $ l^.(L.listElementsL)
      box = case _asMode of
        SelectingItems -> L.renderList (listDrawElement _asRenderer) True l
        _ -> padRight T.Max $ padTop T.Max $ vBox [str " "]
      ui = vBox [ C.hCenter box
                , editorBox _asReplInput _asMode
                , statusBox _asStatus]

editorBox :: E.Editor Text Names -> AppMode -> Widget Names
editorBox e m = hBox [ hLimit 3 $ str "> "
                     , withAttr (editorAttr m) $ E.renderEditor True e ]

editorAttr :: AppMode -> A.AttrName
editorAttr WaitingForIO = editorWaitingAttr
editorAttr _ = editorReadyAttr

statusBox :: Text -> Widget Names
statusBox t = withAttr statusAttr $ txt t

appEvent :: AppState a -> T.BrickEvent Names (AppEvents a) -> T.EventM Names (T.Next (AppState a))
appEvent s@AS{..} (T.VtyEvent e) =
  case (_asMode, e) of
    (_, V.EvKey V.KEsc []) -> M.halt s
    (AcceptingCommands, V.EvKey V.KEnter []) -> do
      let commandText = E.getEditContents _asReplInput
      liftIO $ forkIO $ _asHandler $ concat commandText
      M.continue $ s { _asReplInput = E.applyEdit Z.clearZipper _asReplInput
                     , _asMode = WaitingForIO
                     , _asStatus = ""}
    (AcceptingCommands, e) -> uE $ handleEditorEvents s e
    (SelectingItems, V.EvKey (V.KChar 'q') []) -> M.continue $ s { _asMode = AcceptingCommands }
    (SelectingItems, V.EvKey k _) -> do
      let sel = L.listSelectedElement _asList
      case sel of
        Just (i, a) -> do
          handled <- liftIO $ _asAction k a
          if not handled
            then uL $ handleListEvents s e
            else M.continue s
        Nothing -> uL $ handleListEvents s e
    (SelectingItems, _) -> uL $ handleListEvents s e
    _ -> M.continue s
  where uE m = fmap (updateEditor' s) <$> m
        uL m = fmap (updateList' s) <$> m
appEvent s (T.AppEvent (UpdateStatus t)) = M.continue $ s { _asStatus = t }
appEvent s@AS{..} (T.AppEvent (RenderList action ts)) = M.continue $ s { _asList = newList
                                                                       , _asMode = SelectingItems
                                                                       , _asAction = action }
  where newList = L.listReplace ts (Just 0) _asList
appEvent s@AS{..} (T.AppEvent IOComplete) = case _asMode of
  WaitingForIO -> M.continue $ s { _asMode = AcceptingCommands }
  _ -> M.continue s
appEvent s _ = M.continue s

handleEditorEvents :: AppState a -> V.Event -> T.EventM Names (T.Next (E.Editor Text Names))
handleEditorEvents AS{..} e = E.handleEditorEvent e _asReplInput >>= M.continue

handleListEvents :: AppState a -> V.Event -> T.EventM Names (T.Next (L.List Names a))
handleListEvents AS{..} ev = L.handleListEvent ev _asList >>= M.continue

updateList' s lst = s { _asList = lst }
updateEditor' s e = s { _asReplInput = e }

listDrawElement :: (a -> Text) -> Bool -> a -> Widget Names
listDrawElement fn _ a = padBottom (T.Pad 3) $ txt $ fn a

data Names = TheList | ReplInput | StatusBar
  deriving (Eq, Ord, Show)

data AppMode = AcceptingCommands | SelectingItems | WaitingForIO

type ListAction a = V.Key -> a -> IO Bool

noAction :: ListAction a
noAction _ _ = return False

data AppEvents a = UpdateStatus Text | RenderList (ListAction a) (Vec.Vector a) | IOComplete

type ListRenderer a = a -> Text

data AppState a = AS
  { _asList :: L.List Names a
  , _asRenderer :: ListRenderer a
  , _asAction :: ListAction a
  , _asReplInput :: E.Editor Text Names
  , _asMode :: AppMode
  , _asStatus :: Text
  , _asHandler :: AppHandler
  }

initialState :: AppHandler -> ListRenderer a -> AppState a
initialState h renderer = AS
  { _asList = L.list TheList Vec.empty 1
  , _asRenderer = renderer
  , _asAction = noAction
  , _asReplInput = E.editorText ReplInput renderRepl (Just 1) ""
  , _asMode = AcceptingCommands
  , _asStatus = "accepting commands."
  , _asHandler = h
  }

renderRepl :: [Text] -> Widget Names
renderRepl [] = txt ""
renderRepl (x:_) = txt x

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

statusAttr :: A.AttrName
statusAttr = "statusAttr"

editorReadyAttr, editorWaitingAttr :: A.AttrName
editorReadyAttr = "editorReady"
editorWaitingAttr = "editorWaiting"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.black)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    , (statusAttr,            V.yellow `on` V.black)
    , (editorReadyAttr,       bg V.black)
    , (editorWaitingAttr,     bg V.yellow)
    ]

type AppHandler = Text -> IO ()

theApp :: M.App (AppState a) (AppEvents a) Names
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: BC.BChan (AppEvents a) -> AppHandler -> ListRenderer a -> IO ()
main eventChan handler renderer = void $ M.customMain
  (V.mkVty V.defaultConfig)
  (Just eventChan)
  theApp
  (initialState handler renderer)
