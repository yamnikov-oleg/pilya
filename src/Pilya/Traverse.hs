{-# LANGUAGE FlexibleInstances #-}

module Pilya.Traverse
    ( ASTTraversible (..)
    ) where

import           Control.Monad (forM_)
import qualified Pilya.Syn     as Syn

class ASTTraversible n where
    asttraverse :: (Monad m) => (Syn.Node String -> a -> m a) -> n -> a -> m ()

instance ASTTraversible Syn.NIdentifier where
    asttraverse f node acc = do
        _ <- f node acc
        return ()

instance ASTTraversible Syn.NType where
    asttraverse f node acc = do
        _ <- f (fmap show node) acc
        return ()

instance ASTTraversible Syn.NNumberLiteral where
    asttraverse f node acc = do
        _ <- f (fmap show node) acc
        return ()

instance ASTTraversible Syn.NBoolLiteral where
    asttraverse f node acc = do
        _ <- f (fmap show node) acc
        return ()

instance ASTTraversible Syn.NMultiplier where
    asttraverse f node acc =
        case Syn.nodeValue node of
            Syn.MultIdent ident   -> asttraverse f ident acc
            Syn.MultNumber numlit -> asttraverse f numlit acc
            Syn.MultBool boollit  -> asttraverse f boollit acc
            Syn.MultNot mulr  -> do
                ownAcc <- f (fmap (const "MultNot") node) acc
                asttraverse f mulr ownAcc
            Syn.MultGrouped expr -> asttraverse f expr acc

instance ASTTraversible Syn.NMultOperation where
    asttraverse f node acc = do
        _ <- f (fmap show node) acc
        return ()

instance ASTTraversible Syn.NMultiplication where
    asttraverse f (Syn.Node (Syn.Multiplication mulr pairs) start end) acc = do
        ownAcc <- f (Syn.Node "Multiplication" start end) acc
        asttraverse f mulr ownAcc
        forM_ pairs (\(op, othMulr) -> do
            asttraverse f op ownAcc
            asttraverse f othMulr ownAcc)

instance ASTTraversible Syn.NSumOperation where
    asttraverse f node acc = do
        _ <- f (fmap show node) acc
        return ()

instance ASTTraversible Syn.NSummation where
    asttraverse f (Syn.Node (Syn.Summation muln pairs) start end) acc = do
        ownAcc <- f (Syn.Node "Summation" start end) acc
        asttraverse f muln ownAcc
        forM_ pairs (\(op, othMuln) -> do
            asttraverse f op ownAcc
            asttraverse f othMuln ownAcc)

instance ASTTraversible Syn.NLogOperation where
    asttraverse f node acc = do
        _ <- f (fmap show node) acc
        return ()

instance ASTTraversible Syn.NExpression where
    asttraverse f (Syn.Node (Syn.Expression sumn pairs) start end) acc = do
        ownAcc <- f (Syn.Node "Expression" start end) acc
        asttraverse f sumn ownAcc
        forM_ pairs (\(op, othSumn) -> do
            asttraverse f op ownAcc
            asttraverse f othSumn ownAcc)

instance ASTTraversible Syn.NStatement where
    asttraverse f (Syn.Node (Syn.StmtCompound stmts) start end) acc = do
        ownAcc <- f (Syn.Node "StmtCompound" start end) acc
        forM_ stmts (\stmt -> asttraverse f stmt ownAcc)
    asttraverse f (Syn.Node (Syn.StmtAssignment ident expr) start end) acc = do
        ownAcc <- f (Syn.Node "StmtAssignment" start end) acc
        asttraverse f ident ownAcc
        asttraverse f expr ownAcc
    asttraverse f (Syn.Node (Syn.StmtCondition cond thenBranch mElseBranch) start end) acc = do
        ownAcc <- f (Syn.Node "StmtCondition" start end) acc
        asttraverse f cond ownAcc
        asttraverse f thenBranch ownAcc
        case mElseBranch of
            Just elseBranch -> asttraverse f elseBranch ownAcc
            Nothing         -> return ()
    asttraverse f (Syn.Node (Syn.StmtForLoop ident from to body) start end) acc = do
        ownAcc <- f (Syn.Node "StmtForLoop" start end) acc
        asttraverse f ident ownAcc
        asttraverse f from ownAcc
        asttraverse f to ownAcc
        asttraverse f body ownAcc
    asttraverse f (Syn.Node (Syn.StmtWhileLoop cond body) start end) acc = do
        ownAcc <- f (Syn.Node "StmtWhileLoop" start end) acc
        asttraverse f cond ownAcc
        asttraverse f body ownAcc
    asttraverse f (Syn.Node (Syn.StmtRead idents) start end) acc = do
        ownAcc <- f (Syn.Node "StmtRead" start end) acc
        forM_ idents (\ident -> asttraverse f ident ownAcc)
    asttraverse f (Syn.Node (Syn.StmtWrite exprs) start end) acc = do
        ownAcc <- f (Syn.Node "StmtWrite" start end) acc
        forM_ exprs (\expr -> asttraverse f expr ownAcc)

instance ASTTraversible Syn.NBlock where
    asttraverse f (Syn.Node (Syn.BlockDecl idents typ) start end) acc = do
        ownAcc <- f (Syn.Node "BlockDecl" start end) acc
        forM_ idents (\ident -> asttraverse f ident ownAcc)
        asttraverse f typ ownAcc
    asttraverse f (Syn.Node (Syn.BlockStmt stmt) start end) acc =
        asttraverse f stmt acc

instance ASTTraversible Syn.NProgram where
    asttraverse f (Syn.Node (Syn.Program blocks) start end) acc = do
        ownAcc <- f (Syn.Node "Program" start end) acc
        forM_ blocks (\block -> asttraverse f block ownAcc)
