{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad       (when)
import           Data.GI.Base        (AttrOp ((:=)), get, gtypeInt64,
                                      gtypeString, new, on, set)
import           Data.GI.Base.GValue (IsGValue (fromGValue, toGValue))
import           Data.Int            (Int64)
import           Data.Maybe          (fromJust)
import qualified Data.Text           as T
import qualified GI.Gtk              as Gtk
import qualified Pilya.Lex           as Lex

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

data AppUI = AppUI
    { uiWindow       :: Gtk.Window
    , uiSourceEdit   :: Gtk.TextView
    , uiModeTabs     :: Gtk.Notebook
    , uiLexStore     :: Gtk.ListStore
    , uiLexSelection :: Gtk.TreeSelection
    , uiLexButton    :: Gtk.Button
    }

buildListView :: [(T.Text, Gtk.GType)] -> IO (Gtk.ScrolledWindow, Gtk.TreeView, Gtk.ListStore)
buildListView columns = do
    store <- new Gtk.ListStore []
    #setColumnTypes store $ map snd columns

    treeView <- new Gtk.TreeView
        [ #model := store
        , #headersVisible := True
        ]

    mapM_ (\(index, title) -> do
        renderer <- new Gtk.CellRendererText []
        column <- new Gtk.TreeViewColumn []
        #packStart column renderer True
        #addAttribute column renderer "text" index
        #setTitle column title
        #setExpand column True
        #appendColumn treeView column) $ zip [0..] $ map fst columns

    wind <- new Gtk.ScrolledWindow []
    #add wind treeView

    return (wind, treeView, store)

buildUI :: IO AppUI
buildUI = do
    window <- new Gtk.Window
        [ #title := "Pilya Lang"
        , #widthRequest := 800
        , #heightRequest := 600
        ]

    rootPaned <- new Gtk.Paned
        [ #wideHandle := True
        , #orientation := Gtk.OrientationHorizontal
        ]
    #add window rootPaned

    sourceScroll <- new Gtk.ScrolledWindow []
    #pack1 rootPaned sourceScroll True True

    sourceEdit <- new Gtk.TextView
        [ #monospace := True
        , #topMargin := 12
        , #rightMargin := 12
        , #bottomMargin := 12
        , #leftMargin := 12
        ]
    #add sourceScroll sourceEdit

    modeTabs <- new Gtk.Notebook
        [ #widthRequest := 300
        ]
    #pack2 rootPaned modeTabs False True

    lexTabContainer <- new Gtk.Box
        [ #orientation := Gtk.OrientationVertical
        ]
    lexTabLabel <- new Gtk.Label
        [ #label := "Lexics"
        ]
    #appendPage modeTabs lexTabContainer (Just lexTabLabel)

    lexResultsGrid <- new Gtk.Grid []
    #packStart lexTabContainer lexResultsGrid True True 0

    (lexOutputScroll, lexTreeView, lexStore) <- buildListView
        [ ("Name", gtypeString)
        , ("Line", gtypeInt64)
        , ("Pos", gtypeInt64)
        , ("Len", gtypeInt64)
        ]
    set lexOutputScroll
        [ #hexpand := True
        , #vexpand := True
        ]
    #attach lexResultsGrid lexOutputScroll 1 1 1 1
    lexSelection <- #getSelection lexTreeView

    lexButton <- new Gtk.Button
        [ #label := "Split into tokens"
        , #margin := 6
        ]
    #packEnd lexTabContainer lexButton False False 0

    return AppUI
        { uiWindow = window
        , uiSourceEdit = sourceEdit
        , uiModeTabs = modeTabs
        , uiLexStore = lexStore
        , uiLexSelection = lexSelection
        , uiLexButton = lexButton
        }

instance IsGValue [Char] where
    toGValue = toGValue . Just
    fromGValue gv = do
        maybeStr <- fromGValue gv
        return $ fromJust maybeStr

instance IsGValue Int where
    toGValue num = toGValue (fromIntegral num :: Int64)
    fromGValue gv = do
        i64 <- fromGValue gv :: IO Int64
        return $ fromIntegral i64

class ToGValueList a where
    toGValueList :: a -> IO [Gtk.GValue]

instance (IsGValue a, IsGValue b) => ToGValueList (a, b) where
    toGValueList (x1, x2) = do
        gv1 <- toGValue x1
        gv2 <- toGValue x2
        return [gv1, gv2]

instance (IsGValue a, IsGValue b, IsGValue c) => ToGValueList (a, b, c) where
    toGValueList (x1, x2, x3) = do
        gv1 <- toGValue x1
        gv2 <- toGValue x2
        gv3 <- toGValue x3
        return [gv1, gv2, gv3]

instance (IsGValue a, IsGValue b, IsGValue c, IsGValue d) => ToGValueList (a, b, c, d) where
    toGValueList (x1, x2, x3, x4) = do
        gv1 <- toGValue x1
        gv2 <- toGValue x2
        gv3 <- toGValue x3
        gv4 <- toGValue x4
        return [gv1, gv2, gv3, gv4]

addStoreRow :: (ToGValueList v) => Gtk.ListStore -> v -> IO ()
addStoreRow store values = do
    gvals <- toGValueList values
    let indices = take (length gvals) [0..]
    iter <- #append store
    #set store iter indices gvals

onLexButtonClicked :: AppUI -> IO ()
onLexButtonClicked appUI = do
    srcBuffer <- uiSourceEdit appUI `get` #buffer
    source <- fmap fromJust $ srcBuffer `get` #text
    #clear $ uiLexStore appUI

    case Lex.parse source of
        Left (Lex.ParserError line pos msg) ->
            addStoreRow (uiLexStore appUI) (msg, line, pos, 1::Int)
        Right tokens ->
            mapM_ (\(Lex.Token typ line pos len) ->
                addStoreRow (uiLexStore appUI) (show typ, line, pos, len)) tokens

onLexSelectionChanged :: AppUI -> IO ()
onLexSelectionChanged appUI = do
    (selected, _, iter) <- #getSelected $ uiLexSelection appUI
    when selected $ do
        lineGV <- #getValue (uiLexStore appUI) iter 1
        line <- fromGValue lineGV :: IO Int64

        posGV <- #getValue (uiLexStore appUI) iter 2
        pos <- fromGValue posGV :: IO Int64

        lenGV <- #getValue (uiLexStore appUI) iter 3
        len <- fromGValue lenGV :: IO Int64

        buffer <- #getBuffer $ uiSourceEdit appUI
        cursor <- #getIterAtLineOffset buffer (fromIntegral line - 1) (fromIntegral pos - 1)
        cursor2 <- #copy cursor
        _ <- #forwardChars cursor2 (fromIntegral len)
        #selectRange buffer cursor cursor2

        _ <- #scrollToIter (uiSourceEdit appUI) cursor 0 True 0.5 0.5
        return ()

main :: IO ()
main = do
    Gtk.init Nothing
    appUI <- buildUI
    on (uiWindow appUI) #destroy Gtk.mainQuit
    on (uiLexButton appUI) #clicked (onLexButtonClicked appUI)
    on (uiLexSelection appUI) #changed (onLexSelectionChanged appUI)
    #showAll (uiWindow appUI)
    Gtk.main
