{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.GI.Base        (AttrOp ((:=)), get, gtypeString,
                                      gtypeUInt, new, on, set)
import           Data.GI.Base.GValue (IsGValue (toGValue))
import           Data.Int            (Int64)
import           Data.List           (intercalate)
import           Data.Maybe          (fromJust)
import qualified Data.Text           as T
import qualified GI.Gtk              as Gtk
import qualified Pilya.Lex           as Lex

data AppUI = AppUI
    { uiWindow     :: Gtk.Window
    , uiSourceEdit :: Gtk.TextView
    , uiModeTabs   :: Gtk.Notebook
    , uiLexStore   :: Gtk.ListStore
    , uiLexButton  :: Gtk.Button
    }

buildLexTreeView :: IO (Gtk.TreeView, Gtk.ListStore)
buildLexTreeView = do
    store <- new Gtk.ListStore []
    #setColumnTypes store [gtypeString, gtypeUInt, gtypeUInt]

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

    sourceEdit <- new Gtk.TextView
        [ #monospace := True
        , #topMargin := 12
        , #rightMargin := 12
        , #bottomMargin := 12
        , #leftMargin := 12
        ]
    sourceScroll <- new Gtk.ScrolledWindow []
    #add sourceScroll sourceEdit
    #pack1 rootPaned sourceScroll True True

    modeTabs <- new Gtk.Notebook
        [ #widthRequest := 300
        ]
    #pack2 rootPaned modeTabs False True

    lexTabContainer <- new Gtk.Box
        [ #orientation := Gtk.OrientationVertical
        ]
    (lexTreeView, lexStore) <- buildLexTreeView
    lexOutputScroll <- new Gtk.ScrolledWindow []
    #add lexOutputScroll lexTreeView
    #packStart lexTabContainer lexOutputScroll True True 0
    lexButton <- new Gtk.Button
        [ #label := "Split into tokens"
        , #margin := 6
        ]
    #packEnd lexTabContainer lexButton False False 0
    lexTabLabel <- new Gtk.Label
        [ #label := "Lexics"
        ]
    #appendPage modeTabs lexTabContainer (Just lexTabLabel)

    #add window rootPaned

    return AppUI
        { uiWindow = window
        , uiSourceEdit = sourceEdit
        , uiModeTabs = modeTabs
        , uiLexStore = lexStore
        , uiLexButton = lexButton
        }

lexStoreClear :: AppUI -> IO ()
lexStoreClear = #clear . uiLexStore

addLexStoreRow :: (Integral a, Integral b) => AppUI -> String -> a -> b -> IO ()
addLexStoreRow appUI name line pos = do
    let store = uiLexStore appUI
    nameGV <- toGValue $ Just name
    lineGV <- toGValue (fromIntegral line :: Int64)
    posGV <- toGValue (fromIntegral pos :: Int64)
    iter <- #append store
    #set store iter [0, 1, 2] [nameGV, lineGV, posGV]

onLexButtonClicked :: AppUI -> IO ()
onLexButtonClicked appUI = do
    srcBuffer <- uiSourceEdit appUI `get` #buffer
    source <- fmap fromJust $ srcBuffer `get` #text
    lexStoreClear appUI

    case Lex.parse source of
        Left (Lex.ParserError line pos msg) -> do
            let errText = "Error on " ++ show line ++ ":" ++ show pos ++ ":\n" ++ msg
            addLexStoreRow appUI msg (fromIntegral line) (fromIntegral pos)
            return ()
        Right tokens -> do
            mapM_ (\(Lex.Token typ line pos) ->
                addLexStoreRow appUI (show typ) line pos) tokens
            return ()

main :: IO ()
main = do
    Gtk.init Nothing
    appUI <- buildUI
    on (uiWindow appUI) #destroy Gtk.mainQuit
    on (uiLexButton appUI) #clicked (onLexButtonClicked appUI)
    #showAll (uiWindow appUI)
    Gtk.main
