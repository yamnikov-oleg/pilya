{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad       (forM_, when)
import           Data.GI.Base        (AttrOp ((:=)), get, gtypeInt64,
                                      gtypeString, new, on, set)
import           Data.GI.Base.GValue (IsGValue (fromGValue, toGValue))
import           Data.Int            (Int32, Int64)
import           Data.List           (intercalate)
import           Data.Maybe          (fromJust, isJust)
import qualified Data.Text           as T
import qualified GI.Gtk              as Gtk
import qualified Pilya.Compile       as Comp
import qualified Pilya.Lex           as Lex
import qualified Pilya.Syn           as Syn
import qualified Pilya.Table         as Tbl
import           Pilya.Traverse      (ASTTraversible (..))

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

data AppUI = AppUI
    { uiWindow       :: Gtk.Window
    , uiSourceEdit   :: Gtk.TextView
    , uiModeTabs     :: Gtk.Notebook
    , uiLexKwTree    :: Gtk.TreeView
    , uiLexKwStore   :: Gtk.ListStore
    , uiLexOpTree    :: Gtk.TreeView
    , uiLexOpStore   :: Gtk.ListStore
    , uiLexIdTree    :: Gtk.TreeView
    , uiLexIdStore   :: Gtk.ListStore
    , uiLexNumTree   :: Gtk.TreeView
    , uiLexNumStore  :: Gtk.ListStore
    , uiLexStore     :: Gtk.ListStore
    , uiLexSelection :: Gtk.TreeSelection
    , uiLexButton    :: Gtk.Button
    , uiSynStore     :: Gtk.TreeStore
    , uiSynSelection :: Gtk.TreeSelection
    , uiSynButton    :: Gtk.Button
    , uiAsmStore     :: Gtk.TreeStore
    , uiAsmSelection :: Gtk.TreeSelection
    , uiAsmButton    :: Gtk.Button
    }

buildListView :: [(Maybe T.Text, Gtk.GType)] -> IO (Gtk.ScrolledWindow, Gtk.TreeView, Gtk.ListStore)
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
        #setTitle column $ fromJust title
        #setExpand column True
        #appendColumn treeView column) $ filter (isJust . snd) $ zip [0..] $ map fst columns

    wind <- new Gtk.ScrolledWindow []
    #add wind treeView

    return (wind, treeView, store)

buildTreeView :: [(Maybe T.Text, Gtk.GType)] -> IO (Gtk.ScrolledWindow, Gtk.TreeView, Gtk.TreeStore)
buildTreeView columns = do
    store <- new Gtk.TreeStore []
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
        #setTitle column $ fromJust title
        #setExpand column True
        #appendColumn treeView column) $ filter (isJust . snd) $ zip [0..] $ map fst columns

    wind <- new Gtk.ScrolledWindow []
    #add wind treeView

    return (wind, treeView, store)

buildUI :: IO AppUI
buildUI = do
    window <- new Gtk.Window
        [ #title := "Pilya Lang"
        , #widthRequest := 1000
        , #heightRequest := 700
        ]

    rootPaned <- new Gtk.Paned
        [ #wideHandle := True
        , #orientation := Gtk.OrientationHorizontal
        ]
    #add window rootPaned

    sourceScroll <- new Gtk.ScrolledWindow
        [ #widthRequest := 300
        ]
    #pack1 rootPaned sourceScroll True False

    sourceEdit <- new Gtk.TextView
        [ #monospace := True
        , #topMargin := 12
        , #rightMargin := 12
        , #bottomMargin := 12
        , #leftMargin := 12
        ]
    #add sourceScroll sourceEdit

    modeTabs <- new Gtk.Notebook
        [ #widthRequest := 450
        ]
    #pack2 rootPaned modeTabs False False

    lexTabContainer <- new Gtk.Box
        [ #orientation := Gtk.OrientationVertical
        ]
    lexTabLabel <- new Gtk.Label
        [ #label := "Lexics"
        ]
    #appendPage modeTabs lexTabContainer (Just lexTabLabel)

    lexResultsGrid <- new Gtk.Grid
        [ #rowSpacing := 6
        , #columnSpacing := 6
        , #margin := 6
        ]
    #packStart lexTabContainer lexResultsGrid True True 0

    (lexKwScroll, lexKwTreeView, lexKwStore) <- buildListView
        [ (Just "Index", gtypeString)
        , (Just "Content", gtypeString)
        ]
    set lexKwScroll
        [ #hexpand := True
        , #vexpand := True
        ]
    #attach lexResultsGrid lexKwScroll 1 1 1 1

    (lexOpScroll, lexOpTreeView, lexOpStore) <- buildListView
        [ (Just "Index", gtypeString)
        , (Just "Content", gtypeString)
        ]
    set lexOpScroll
        [ #hexpand := True
        , #vexpand := True
        ]
    #attach lexResultsGrid lexOpScroll 2 1 1 1

    (lexIdScroll, lexIdTreeView, lexIdStore) <- buildListView
        [ (Just "Index", gtypeString)
        , (Just "Value", gtypeString)
        ]
    set lexIdScroll
        [ #hexpand := True
        , #vexpand := True
        ]
    #attach lexResultsGrid lexIdScroll 1 2 1 1

    (lexNumScroll, lexNumTreeView, lexNumStore) <- buildListView
        [ (Just "Index", gtypeString)
        , (Just "Value", gtypeString)
        ]
    set lexNumScroll
        [ #hexpand := True
        , #vexpand := True
        ]
    #attach lexResultsGrid lexNumScroll 2 2 1 1

    (lexOutputScroll, lexTreeView, lexStore) <- buildListView
        [ (Just "Name", gtypeString)
        , (Just "Line", gtypeInt64)
        , (Just "Pos", gtypeInt64)
        , (Just "Len", gtypeInt64)
        , (Nothing, gtypeInt64) -- Entry Table
        , (Nothing, gtypeInt64) -- Entry Index
        ]
    set lexOutputScroll
        [ #hexpand := True
        , #vexpand := True
        ]
    #attach lexResultsGrid lexOutputScroll 1 3 2 1
    lexSelection <- #getSelection lexTreeView

    lexButton <- new Gtk.Button
        [ #label := "Split into tokens"
        , #margin := 6
        ]
    #packEnd lexTabContainer lexButton False False 0

    synTabContainer <- new Gtk.Box
        [ #orientation := Gtk.OrientationVertical
        ]
    synTabLabel <- new Gtk.Label
        [ #label := "Syntax"
        ]
    #appendPage modeTabs synTabContainer (Just synTabLabel)

    (synOutputScroll, synTreeView, synStore) <- buildTreeView
        [ (Just "Name", gtypeString)
        , (Nothing, gtypeInt64)
        , (Nothing, gtypeInt64)
        , (Nothing, gtypeInt64)
        , (Nothing, gtypeInt64)
        , (Just "Pos", gtypeString)
        ]
    set lexOutputScroll
        [ #hexpand := True
        , #vexpand := True
        ]
    #packStart synTabContainer synOutputScroll True True 0
    synSelection <- #getSelection synTreeView

    synButton <- new Gtk.Button
        [ #label := "Parse into AST"
        , #margin := 6
        ]
    #packEnd synTabContainer synButton False False 0

    asmTabContainer <- new Gtk.Box
        [ #orientation := Gtk.OrientationVertical
        ]
    asmTabLabel <- new Gtk.Label
        [ #label := "Assembly"
        ]
    #appendPage modeTabs asmTabContainer (Just asmTabLabel)

    (asmOutputScroll, asmTreeView, asmStore) <- buildTreeView
        [ (Just "I", gtypeString)
        , (Just "Name", gtypeString)
        , (Just "Pos", gtypeString)
        , (Nothing, gtypeInt64)
        , (Nothing, gtypeInt64)
        ]
    set asmOutputScroll
        [ #hexpand := True
        , #vexpand := True
        ]
    #packStart asmTabContainer asmOutputScroll True True 0
    asmSelection <- #getSelection asmTreeView

    asmButton <- new Gtk.Button
        [ #label := "Compile into assembly"
        , #margin := 6
        ]
    #packEnd asmTabContainer asmButton False False 0

    return AppUI
        { uiWindow = window
        , uiSourceEdit = sourceEdit
        , uiModeTabs = modeTabs
        , uiLexKwTree = lexKwTreeView
        , uiLexKwStore = lexKwStore
        , uiLexOpTree = lexOpTreeView
        , uiLexOpStore = lexOpStore
        , uiLexIdTree = lexIdTreeView
        , uiLexIdStore = lexIdStore
        , uiLexNumTree = lexNumTreeView
        , uiLexNumStore = lexNumStore
        , uiLexStore = lexStore
        , uiLexSelection = lexSelection
        , uiLexButton = lexButton
        , uiSynStore = synStore
        , uiSynSelection = synSelection
        , uiSynButton = synButton
        , uiAsmStore = asmStore
        , uiAsmSelection = asmSelection
        , uiAsmButton = asmButton
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

instance (IsGValue a, IsGValue b, IsGValue c, IsGValue d, IsGValue e) => ToGValueList (a, b, c, d, e) where
    toGValueList (x1, x2, x3, x4, x5) = do
        gv1 <- toGValue x1
        gv2 <- toGValue x2
        gv3 <- toGValue x3
        gv4 <- toGValue x4
        gv5 <- toGValue x5
        return [gv1, gv2, gv3, gv4, gv5]

instance (IsGValue a, IsGValue b, IsGValue c, IsGValue d, IsGValue e, IsGValue f) => ToGValueList (a, b, c, d, e, f) where
    toGValueList (x1, x2, x3, x4, x5, x6) = do
        gv1 <- toGValue x1
        gv2 <- toGValue x2
        gv3 <- toGValue x3
        gv4 <- toGValue x4
        gv5 <- toGValue x5
        gv6 <- toGValue x6
        return [gv1, gv2, gv3, gv4, gv5, gv6]

addStoreRow :: (ToGValueList v) => Gtk.ListStore -> v -> IO ()
addStoreRow store values = do
    gvals <- toGValueList values
    let indices = take (length gvals) [0..]
    iter <- #append store
    #set store iter indices gvals

fillLexStoreFromTable :: (Show a) => Gtk.ListStore -> Int -> Tbl.Table a -> IO ()
fillLexStoreFromTable store tblIndex tbl = do
    let enumerated = zip ([0..] :: [Int]) $ Tbl.toList tbl
    let toRow (ind, value) = (show (tblIndex, ind), show value)
    mapM_ (addStoreRow store . toRow) enumerated

onLexButtonClicked :: AppUI -> IO ()
onLexButtonClicked appUI = do
    srcBuffer <- uiSourceEdit appUI `get` #buffer
    source <- fmap fromJust $ srcBuffer `get` #text
    #clear $ uiLexStore appUI
    #clear $ uiLexKwStore appUI
    #clear $ uiLexOpStore appUI
    #clear $ uiLexIdStore appUI
    #clear $ uiLexNumStore appUI

    case Lex.parse source of
        Left (Lex.ParserError line pos msg) ->
            addStoreRow (uiLexStore appUI) (msg, line, pos, 1::Int, -1::Int, -1::Int)
        Right tokens -> do
            let (Lex.ParserEntries idTable numTable entries) = Lex.toParserEntries tokens

            fillLexStoreFromTable (uiLexKwStore appUI) 0 Lex.keywordsTable
            fillLexStoreFromTable (uiLexOpStore appUI) 1 Lex.operatorTable
            fillLexStoreFromTable (uiLexIdStore appUI) 2 idTable
            fillLexStoreFromTable (uiLexNumStore appUI) 3 numTable

            mapM_ (\(Lex.Entry line pos len tbl ind) ->
                addStoreRow (uiLexStore appUI) (show (tbl, ind), line, pos, len, tbl, ind)) entries

getLexTreeByIndex :: AppUI -> Int -> Maybe Gtk.TreeView
getLexTreeByIndex appUI 0 = Just $ uiLexKwTree appUI
getLexTreeByIndex appUI 1 = Just $ uiLexOpTree appUI
getLexTreeByIndex appUI 2 = Just $ uiLexIdTree appUI
getLexTreeByIndex appUI 3 = Just $ uiLexNumTree appUI
getLexTreeByIndex _ _     = Nothing

lexUnselectAll :: AppUI -> IO ()
lexUnselectAll appUI = do
    kwSel <- #getSelection $ uiLexKwTree appUI
    #unselectAll kwSel

    opSel <- #getSelection $ uiLexOpTree appUI
    #unselectAll opSel

    idSel <- #getSelection $ uiLexIdTree appUI
    #unselectAll idSel

    numSel <- #getSelection $ uiLexNumTree appUI
    #unselectAll numSel

selectNth :: Gtk.TreeView -> Int -> IO ()
selectNth treeView n = do
    mstore <- #getModel treeView
    case mstore of
        Just store -> do
            (exists, nthIter) <- #iterNthChild store Nothing (fromIntegral n)
            when exists $ do
                selection <- #getSelection treeView
                #selectIter selection nthIter
                path <- #getPath store nthIter
                #scrollToCell treeView (Just path) (Nothing :: Maybe Gtk.TreeViewColumn) False 0.5 0.5
        Nothing -> return ()

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

        tblGV <- #getValue (uiLexStore appUI) iter 4
        tbl <- fromGValue tblGV :: IO Int64

        indGV <- #getValue (uiLexStore appUI) iter 5
        ind <- fromGValue indGV :: IO Int64

        buffer <- #getBuffer $ uiSourceEdit appUI
        cursor <- #getIterAtLineOffset buffer (fromIntegral line - 1) (fromIntegral pos - 1)
        cursor2 <- #copy cursor
        _ <- #forwardChars cursor2 (fromIntegral len)
        #selectRange buffer cursor cursor2

        let maybeStore = getLexTreeByIndex appUI $ fromIntegral tbl
        when (isJust maybeStore) (lexUnselectAll appUI)
        forM_ maybeStore (`selectNth` fromIntegral ind)

        _ <- #scrollToIter (uiSourceEdit appUI) cursor 0 True 0.5 0.5
        return ()

synDisplayNode :: Syn.Node String -> (Gtk.TreeStore, Maybe Gtk.TreeIter) -> IO (Gtk.TreeStore, Maybe Gtk.TreeIter)
synDisplayNode (Syn.Node text start end) (store, piter) = do
    let Syn.Cursor startLine startPos = start
    let Syn.Cursor endLine endPos = end
    let posStr = show startLine ++ "," ++ show startPos ++ " - " ++ show endLine ++ "," ++ show endPos
    iter <- treeStoreAppend store piter (text, startLine, startPos, endLine, endPos, posStr)
    return (store, Just iter)

onSynButtonClicked :: AppUI -> IO ()
onSynButtonClicked appUI = do
    #clear $ uiSynStore appUI

    srcBuffer <- uiSourceEdit appUI `get` #buffer
    source <- fmap fromJust $ srcBuffer `get` #text

    case Lex.parse source of
        Left (Lex.ParserError line pos msg) -> do
            let posStr = show line ++ "," ++ show pos
            _ <- treeStoreAppend (uiSynStore appUI) Nothing (msg, line, pos, line, pos+1, posStr)
            return ()
        Right tokens ->
            case Syn.parse tokens of
                Left (Syn.ParserError (Syn.ErrorMsg msg) line pos) -> do
                    let posStr = show line ++ "," ++ show pos
                    _ <- treeStoreAppend (uiSynStore appUI) Nothing (msg, line, pos, line, pos+1, posStr)
                    return ()
                Right program ->
                     asttraverse synDisplayNode program (uiSynStore appUI, Nothing)

getIterAt :: (Integral l, Integral o) => Gtk.TextBuffer -> l -> o -> IO Gtk.TextIter
getIterAt buffer line offset = do
    let line' = if line < 0 then 0 else line
    cursor <- #getIterAtLine buffer (fromIntegral line)
    lineLen <- #getCharsInLine cursor
    let offset' = if offset < 0 then 0 else offset
    let offset'' = if fromIntegral offset' > lineLen
            then lineLen
            else fromIntegral offset'
    #setLineOffset cursor offset''
    return cursor

onSynSelectionChanged :: AppUI -> IO ()
onSynSelectionChanged appUI = do
    (selected, _, iter) <- #getSelected $ uiSynSelection appUI
    when selected $ do
        slGV <- #getValue (uiSynStore appUI) iter 1
        startLine <- fromGValue slGV :: IO Int64

        spGV <- #getValue (uiSynStore appUI) iter 2
        startPos <- fromGValue spGV :: IO Int64

        elGV <- #getValue (uiSynStore appUI) iter 3
        endLine <- fromGValue elGV :: IO Int64

        epGV <- #getValue (uiSynStore appUI) iter 4
        endPos <- fromGValue epGV :: IO Int64

        buffer <- #getBuffer $ uiSourceEdit appUI
        cursor <- getIterAt buffer (startLine - 1) (startPos - 1)
        cursor2 <- getIterAt buffer (endLine - 1) (endPos - 1)
        #selectRange buffer cursor cursor2

        _ <- #scrollToIter (uiSourceEdit appUI) cursor 0 True 0.5 0.5
        return ()
    return ()

onAsmButtonClicked :: AppUI -> IO ()
onAsmButtonClicked appUI = do
    #clear $ uiAsmStore appUI

    srcBuffer <- uiSourceEdit appUI `get` #buffer
    source <- fmap fromJust $ srcBuffer `get` #text

    case Lex.parse source of
        Left (Lex.ParserError line pos msg) -> do
            let posStr = show line ++ "," ++ show pos
            _ <- treeStoreAppend (uiAsmStore appUI) Nothing ("L"::String, msg, posStr, line, pos)
            return ()
        Right tokens ->
            case Syn.parse tokens of
                Left (Syn.ParserError (Syn.ErrorMsg msg) line pos) -> do
                    let posStr = show line ++ "," ++ show pos
                    _ <- treeStoreAppend (uiAsmStore appUI) Nothing ("S"::String, msg, posStr, line, pos)
                    return ()
                Right program ->
                     case Comp.compile program of
                        Left (Comp.Error cur msg) -> do
                            let posStr = show cur
                            _ <- treeStoreAppend (uiAsmStore appUI) Nothing ("C"::String, msg, posStr, Syn.cursorLine cur, Syn.cursorPos cur)
                            return ()
                        Right instr ->
                            forM_ (Tbl.toEnumList instr) (\(ind, (line, ins)) ->
                                treeStoreAppend (uiAsmStore appUI) Nothing (show ind, show ins, show line, line, -1::Int))

onAsmSelectionChanged :: AppUI -> IO ()
onAsmSelectionChanged appUI = do
    (selected, _, iter) <- #getSelected $ uiAsmSelection appUI
    when selected $ do
        lineGV <- #getValue (uiAsmStore appUI) iter 3
        line <- fromGValue lineGV :: IO Int64

        posGV <- #getValue (uiAsmStore appUI) iter 4
        pos <- fromGValue posGV :: IO Int64

        buffer <- #getBuffer $ uiSourceEdit appUI
        (cursor, cursor2) <- if pos >= 0
        then do
            cursor <- #getIterAtLineOffset buffer (fromIntegral line - 1) (fromIntegral pos - 1)
            cursor2 <- #copy cursor
            _ <- #forwardChars cursor2 1
            return (cursor, cursor2)
        else do
            cursor <- #getIterAtLineOffset buffer (fromIntegral line - 1) 0
            cursor2 <- #copy cursor
            _ <- #forwardToLineEnd cursor2
            return (cursor, cursor2)

        #selectRange buffer cursor cursor2
        _ <- #scrollToIter (uiSourceEdit appUI) cursor 0 True 0.5 0.5
        return ()
    return ()

treeStoreAppend :: (ToGValueList v) => Gtk.TreeStore -> Maybe Gtk.TreeIter -> v -> IO Gtk.TreeIter
treeStoreAppend store maybeParent vals = do
    gvals <- toGValueList vals
    let indices = take (length gvals) [0..]
    iter <- #append store maybeParent
    #set store iter indices gvals
    return iter

main :: IO ()
main = do
    Gtk.init Nothing
    appUI <- buildUI
    on (uiWindow appUI) #destroy Gtk.mainQuit
    on (uiLexButton appUI) #clicked (onLexButtonClicked appUI)
    on (uiLexSelection appUI) #changed (onLexSelectionChanged appUI)
    on (uiSynButton appUI) #clicked (onSynButtonClicked appUI)
    on (uiSynSelection appUI) #changed (onSynSelectionChanged appUI)
    on (uiAsmButton appUI) #clicked (onAsmButtonClicked appUI)
    on (uiAsmSelection appUI) #changed (onAsmSelectionChanged appUI)
    #showAll (uiWindow appUI)
    Gtk.main
