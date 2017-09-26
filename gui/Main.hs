{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad       (when)
import           Data.GI.Base        (AttrOp ((:=)), get, gtypeInt64,
                                      gtypeString, new, on, set)
import           Data.GI.Base.GValue (IsGValue (fromGValue, toGValue))
import           Data.Int            (Int64)
import           Data.Maybe          (fromJust)
import qualified Data.Text           as T
import qualified GI.Gtk              as Gtk
import qualified Pilya.Lex           as Lex

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

addLexStoreRow :: (Integral a, Integral b, Integral c) =>
    AppUI -> String -> a -> b -> c -> IO ()
addLexStoreRow appUI name line pos len = do
    let store = uiLexStore appUI
    nameGV <- toGValue $ Just name
    lineGV <- toGValue (fromIntegral line :: Int64)
    posGV <- toGValue (fromIntegral pos :: Int64)
    lenGV <- toGValue (fromIntegral len :: Int64)
    iter <- #append store
    #set store iter [0, 1, 2, 3] [nameGV, lineGV, posGV, lenGV]

onLexButtonClicked :: AppUI -> IO ()
onLexButtonClicked appUI = do
    srcBuffer <- uiSourceEdit appUI `get` #buffer
    source <- fmap fromJust $ srcBuffer `get` #text
    #clear $ uiLexStore appUI

    case Lex.parse source of
        Left (Lex.ParserError line pos msg) ->
            addLexStoreRow appUI msg line pos (1::Int64)
        Right tokens ->
            mapM_ (\(Lex.Token typ line pos len) ->
                addLexStoreRow appUI (show typ) line pos len) tokens

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
