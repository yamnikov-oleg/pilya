{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad       (when)
import           Data.GI.Base        (AttrOp ((:=)), get, gtypeInt64,
                                      gtypeString, new, on)
import           Data.GI.Base.GValue (IsGValue (fromGValue, toGValue))
import           Data.Int            (Int64)
import           Data.Maybe          (fromJust)
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

buildLexTreeView :: IO (Gtk.TreeView, Gtk.ListStore)
buildLexTreeView = do
    store <- new Gtk.ListStore []
    #setColumnTypes store [gtypeString, gtypeInt64, gtypeInt64, gtypeInt64]

    treeView <- new Gtk.TreeView
        [ #model := store
        , #headersVisible := True
        ]

    nameRenderer <- new Gtk.CellRendererText []
    nameColumn <- new Gtk.TreeViewColumn []
    #packStart nameColumn nameRenderer True
    #addAttribute nameColumn nameRenderer "text" 0
    #setTitle nameColumn "Name"
    #setExpand nameColumn True
    #appendColumn treeView nameColumn

    lineRenderer <- new Gtk.CellRendererText []
    lineColumn <- new Gtk.TreeViewColumn []
    #packStart lineColumn lineRenderer True
    #addAttribute lineColumn lineRenderer "text" 1
    #setTitle lineColumn "Line"
    #appendColumn treeView lineColumn

    posRenderer <- new Gtk.CellRendererText []
    posColumn <- new Gtk.TreeViewColumn []
    #packStart posColumn posRenderer True
    #addAttribute posColumn posRenderer "text" 2
    #setTitle posColumn "Pos"
    #appendColumn treeView posColumn

    lenRenderer <- new Gtk.CellRendererText []
    lenColumn <- new Gtk.TreeViewColumn []
    #packStart lenColumn lenRenderer True
    #addAttribute lenColumn lenRenderer "text" 3
    #setTitle lenColumn "Len"
    #appendColumn treeView lenColumn

    return (treeView, store)

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

    lexOutputScroll <- new Gtk.ScrolledWindow
        [ #hexpand := True
        , #vexpand := True
        ]
    #attach lexResultsGrid lexOutputScroll 1 1 1 1

    (lexTreeView, lexStore) <- buildLexTreeView
    lexSelection <- #getSelection lexTreeView
    #add lexOutputScroll lexTreeView

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

lexStoreClear :: AppUI -> IO ()
lexStoreClear = #clear . uiLexStore

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
    lexStoreClear appUI

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
