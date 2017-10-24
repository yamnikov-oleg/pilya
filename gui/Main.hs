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
import qualified Pilya.Lex           as Lex
import qualified Pilya.Syn           as Syn
import qualified Pilya.Table         as Tbl

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
    , uiSynButton    :: Gtk.Button
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
        , (Just "Line", gtypeInt64)
        , (Just "Pos", gtypeInt64)
        ]
    set lexOutputScroll
        [ #hexpand := True
        , #vexpand := True
        ]
    #packStart synTabContainer synOutputScroll True True 0

    synButton <- new Gtk.Button
        [ #label := "Parse into AST"
        , #margin := 6
        ]
    #packEnd synTabContainer synButton False False 0

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
        , uiSynButton = synButton
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

synDisplayMulr :: Gtk.TreeStore -> Gtk.TreeIter -> Syn.Multiplier -> IO ()
synDisplayMulr store parentIter (Syn.MultIdent ident) = do
    let text = "Ident " ++ ident
    _ <- treeStoreAppend store (Just parentIter) (text, 0 :: Int, 0 :: Int)
    return ()
synDisplayMulr store parentIter (Syn.MultNumber lit) = do
    let text = case lit of
            Syn.NumInteger int -> "Integer " ++ show int
            Syn.NumReal real   -> "Real " ++ show real
    _ <- treeStoreAppend store (Just parentIter) (text, 0 :: Int, 0 :: Int)
    return ()
synDisplayMulr store parentIter (Syn.MultBool lit) = do
    let text = case lit of
            Syn.BoolTrue  -> "True" :: String
            Syn.BoolFalse -> "False" :: String
    _ <- treeStoreAppend store (Just parentIter) (text, 0 :: Int, 0 :: Int)
    return ()
synDisplayMulr store parentIter (Syn.MultNot mulr) = do
    iter <- treeStoreAppend store (Just parentIter) ("Not" :: String, 0 :: Int, 0 :: Int)
    synDisplayMulr store iter mulr
synDisplayMulr store parentIter (Syn.MultGrouped expr) = do
    iter <- treeStoreAppend store (Just parentIter) ("Group" :: String, 0 :: Int, 0 :: Int)
    synDisplayExpression store iter expr

synDisplayMultOpPair :: Gtk.TreeStore -> Gtk.TreeIter -> (Syn.MultOperation, Syn.Multiplier) -> IO ()
synDisplayMultOpPair store parentIter (op, mulr) = do
    _ <- treeStoreAppend store (Just parentIter) (show op, 0 :: Int, 0 :: Int)
    synDisplayMulr store parentIter mulr
    return ()

synDisplayMult :: Gtk.TreeStore -> Gtk.TreeIter -> Syn.Multiplication -> IO ()
synDisplayMult store parentIter (Syn.Multiplication mulr opPairs) = do
    iter <- treeStoreAppend store (Just parentIter) ("Multiplication" :: String, 0 :: Int, 0 :: Int)
    synDisplayMulr store iter mulr
    forM_ opPairs (synDisplayMultOpPair store iter)

synDisplaySumOpPair :: Gtk.TreeStore -> Gtk.TreeIter -> (Syn.SumOperation, Syn.Multiplication) -> IO ()
synDisplaySumOpPair store parentIter (op, mult) = do
    _ <- treeStoreAppend store (Just parentIter) (show op, 0 :: Int, 0 :: Int)
    synDisplayMult store parentIter mult
    return ()

synDisplaySummation :: Gtk.TreeStore -> Gtk.TreeIter -> Syn.Summation -> IO ()
synDisplaySummation store parentIter (Syn.Summation mult opPairs) = do
    iter <- treeStoreAppend store (Just parentIter) ("Summation" :: String, 0 :: Int, 0 :: Int)
    synDisplayMult store iter mult
    forM_ opPairs (synDisplaySumOpPair store iter)

synDisplayLogOpPair :: Gtk.TreeStore -> Gtk.TreeIter -> (Syn.LogOperation, Syn.Summation) -> IO ()
synDisplayLogOpPair store parentIter (op, summ) = do
    _ <- treeStoreAppend store (Just parentIter) (show op, 0 :: Int, 0 :: Int)
    synDisplaySummation store parentIter summ
    return ()

synDisplayExpression :: Gtk.TreeStore -> Gtk.TreeIter -> Syn.Expression -> IO ()
synDisplayExpression store parentIter (Syn.Expression summ opPairs) = do
    iter <- treeStoreAppend store (Just parentIter) ("Expression" :: String, 0 :: Int, 0 :: Int)
    synDisplaySummation store iter summ
    forM_ opPairs (synDisplayLogOpPair store iter)

synDisplayStatement :: Gtk.TreeStore -> Gtk.TreeIter -> Syn.Statement -> IO ()
synDisplayStatement store parentIter (Syn.StmtAssignment ident expr) = do
    let text = "Assignment " ++ ident
    iter <- treeStoreAppend store (Just parentIter) (text, 0 :: Int, 0 :: Int)
    synDisplayExpression store iter expr
    return ()
synDisplayStatement store parentIter (Syn.StmtCondition cond thenBranch mElseBranch) = do
    iter <- treeStoreAppend store (Just parentIter) ("Condition" :: String, 0 :: Int, 0 :: Int)
    synDisplayExpression store iter cond
    synDisplayStatement store iter thenBranch
    case mElseBranch of
        Just elseBranch -> synDisplayStatement store iter elseBranch
        Nothing         -> return ()
synDisplayStatement store parentIter (Syn.StmtForLoop var from to body) = do
    let text = "ForLoop " ++ var
    iter <- treeStoreAppend store (Just parentIter) (text, 0 :: Int, 0 :: Int)
    synDisplayExpression store iter from
    synDisplayExpression store iter to
    synDisplayStatement store iter body
synDisplayStatement store parentIter (Syn.StmtWhileLoop cond body) = do
    iter <- treeStoreAppend store (Just parentIter) ("WhileLoop" :: String, 0 :: Int, 0 :: Int)
    synDisplayExpression store iter cond
    synDisplayStatement store iter body
synDisplayStatement store parentIter (Syn.StmtRead idents) = do
    let text = "Read " ++ intercalate ", " (idents::[String])
    _ <- treeStoreAppend store (Just parentIter) (text, 0 :: Int, 0 :: Int)
    return ()
synDisplayStatement store parentIter (Syn.StmtWrite exprs) = do
    iter <- treeStoreAppend store (Just parentIter) ("Write" :: String, 0 :: Int, 0 :: Int)
    forM_ exprs (synDisplayExpression store iter)
synDisplayStatement store parentIter (Syn.StmtCompound stmts) = do
    iter <- treeStoreAppend store (Just parentIter) ("Compound" :: String, 0 :: Int, 0 :: Int)
    forM_ stmts (synDisplayStatement store iter)

synDisplayBlock :: Gtk.TreeStore -> Gtk.TreeIter -> Syn.Block -> IO ()
synDisplayBlock store parentIter (Syn.BlockDecl idents typ) = do
    let text = "Declaration " ++ intercalate ", " (idents::[String]) ++ " : " ++ show typ
    _ <- treeStoreAppend store (Just parentIter) (text, 0 :: Int, 0 :: Int)
    return ()
synDisplayBlock store parentIter (Syn.BlockStmt stmt) =
    synDisplayStatement store parentIter stmt

synDisplayProgram :: Gtk.TreeStore -> Syn.Program -> IO ()
synDisplayProgram store (Syn.Program blocks) = do
    iter <- treeStoreAppend store Nothing ("Program" :: String, 0 :: Int, 0 :: Int)
    forM_ blocks (synDisplayBlock store iter)

onSynButtonClicked :: AppUI -> IO ()
onSynButtonClicked appUI = do
    #clear $ uiSynStore appUI

    srcBuffer <- uiSourceEdit appUI `get` #buffer
    source <- fmap fromJust $ srcBuffer `get` #text

    case Lex.parse source of
        Left (Lex.ParserError line pos msg) -> do
            _ <- treeStoreAppend (uiSynStore appUI) Nothing (msg, line, pos)
            return ()
        Right tokens ->
            case Syn.parse tokens of
                Left (Syn.ParserError (Syn.ErrorMsg msg) line pos) -> do
                    _ <- treeStoreAppend (uiSynStore appUI) Nothing (msg, line, pos)
                    return ()
                Right program ->
                    synDisplayProgram (uiSynStore appUI) program

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
    #showAll (uiWindow appUI)
    Gtk.main
