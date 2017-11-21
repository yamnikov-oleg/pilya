{-# LANGUAGE DeriveFunctor #-}
module Pilya.Compile
    ( Error (..)
    , compile
    ) where

import           Control.Monad (foldM, forM_, mapM, when)
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import qualified Pilya.Exec    as Exec
import qualified Pilya.Syn     as Syn
import           Pilya.Table   (Table)
import qualified Pilya.Table   as Tbl

data Error = Error
    { errorCursor :: Syn.Cursor
    , errorMsg    :: String
    }

data Type = TypeInt | TypeReal | TypeBool
    deriving (Show, Eq)

typeOf :: Exec.Value -> Type
typeOf (Exec.ValInt _)  = TypeInt
typeOf (Exec.ValReal _) = TypeReal
typeOf (Exec.ValBool _) = TypeBool

defaultOf :: Type -> Exec.Value
defaultOf TypeInt  = Exec.ValInt 0
defaultOf TypeReal = Exec.ValReal 0.0
defaultOf TypeBool = Exec.ValBool False

fromSynType :: Syn.Type -> Type
fromSynType Syn.TypeInt  = TypeInt
fromSynType Syn.TypeReal = TypeReal
fromSynType Syn.TypeBool = TypeBool

data State = State
    { stInstr     :: Table (Int, Exec.Instruction)
    , stTmpVarCnt :: Integer
    , stTypeMap   :: Map String Type
    }

newState :: State
newState = State
    { stInstr = Tbl.empty
    , stTmpVarCnt = 0
    , stTypeMap = M.empty
    }

newtype Compiler a = Compiler (State -> Either Error (a, State))
    deriving (Functor)

runCompiler :: Compiler a -> State -> Either Error (a, State)
runCompiler (Compiler f) = f

instance Applicative Compiler where
    pure x = Compiler (\s -> Right (x, s))
    cf <*> cx = Compiler (\s1 -> do
        (f, s2) <- runCompiler cf s1
        (x, s3) <- runCompiler cx s2
        return (f x, s3))

instance Monad Compiler where
    return = pure
    (Compiler cf) >>= f = Compiler (\st -> do
        (val, st2) <- cf st
        let Compiler cf2 = f val
        cf2 st2)

cerror :: Syn.Cursor -> String -> Compiler ()
cerror cur msg = Compiler (\_ -> Left $ Error cur msg)

ins :: Int -> Exec.Instruction -> Compiler Int
ins line i = Compiler (\(State is cnt tm) ->
    let (ind, is2) = Tbl.append is (line, i) in
    Right (ind, State is2 cnt tm))

setins :: Int -> Int -> Exec.Instruction -> Compiler ()
setins ind line ins = Compiler (\(State is cnt tm) ->
    Right ((), State (Tbl.set is ind (line, ins)) cnt tm))

tmpcnt :: Compiler Integer
tmpcnt = Compiler (\st@(State _ cnt _) -> Right (cnt, st))

inctmpcnt :: Compiler ()
inctmpcnt = Compiler (\(State is tc tm) -> Right ((), State is (tc + 1) tm))

isdefined :: String -> Compiler Bool
isdefined varname = Compiler (\st@(State _ _ tm) ->
    case M.lookup varname tm of
        Just _  -> Right (True, st)
        Nothing -> Right (False, st))

gettype :: String -> Syn.Cursor -> Compiler Type
gettype varname cur = Compiler (\st@(State _ _ tm) ->
    case M.lookup varname tm of
        Just t -> Right (t, st)
        Nothing -> Left $ Error cur ("Undefined variable '" ++ varname ++ "'"))

settype :: String -> Type -> Compiler ()
settype varname tp = Compiler (\(State is tc tm) ->
    Right ((), State is tc (M.insert varname tp tm)))

tmpvar :: Int -> Exec.Value -> Compiler String
tmpvar line val = do
    tmpcnt <- tmpcnt
    let varname = "$t" ++ show (tmpcnt + 1)
    ins line $ Exec.IDecl varname val
    inctmpcnt
    settype varname (typeOf val)
    return varname

compileMultiplier :: Syn.NMultiplier -> Compiler (String, Type)
compileMultiplier (Syn.Node (Syn.MultIdent ident) start _) = do
    tp <- gettype (Syn.nodeValue ident) (Syn.nodeStart ident)
    return (Syn.nodeValue ident, tp)
compileMultiplier (Syn.Node (Syn.MultNumber num) start _) = do
    let line = Syn.cursorLine start
    case Syn.nodeValue num of
        Syn.NumInteger int -> do
            varname <- tmpvar line (Exec.ValInt int)
            return (varname, TypeInt)
        Syn.NumReal dbl -> do
            varname <- tmpvar line (Exec.ValReal dbl)
            return (varname, TypeReal)
compileMultiplier (Syn.Node (Syn.MultBool bool) start _) = do
    let line = Syn.cursorLine start
    let b = case Syn.nodeValue bool of
            Syn.BoolTrue  -> True
            Syn.BoolFalse -> False
    varname <- tmpvar line (Exec.ValBool b)
    return (varname, TypeBool)
compileMultiplier (Syn.Node (Syn.MultNot mulr) start _) = do
    let line = Syn.cursorLine start
    (varname, tp) <- compileMultiplier mulr
    when (tp /= TypeBool) $
        cerror start $ "Can't apply 'not' operator to a value of type " ++ show tp
    outname <- tmpvar line (defaultOf TypeBool)
    ins line (Exec.INot varname outname)
    return (outname, TypeBool)
compileMultiplier (Syn.Node (Syn.MultGrouped expr) _ _) =
    compileExpression expr

compileMultiplication :: Syn.NMultiplication -> Compiler (String, Type)
compileMultiplication (Syn.Node (Syn.Multiplication mulr oppairs) start _) = do
    let line = Syn.cursorLine start
    (var1, typ) <- compileMultiplier mulr

    if null oppairs
    then
        return (var1, typ)
    else do
        outvar <- tmpvar line $ defaultOf typ
        ins line (Exec.IMov var1 outvar)
        forM_ oppairs (\(nop, mul) -> do
            let (Syn.Node op opstart _) = nop
            (varx, typx) <- compileMultiplier mul
            case (op, typ, typx) of
                (Syn.MultOpMult, TypeInt, TypeInt) -> do
                    ins line $ Exec.IMulI outvar varx outvar
                    return ()
                (Syn.MultOpMult, TypeReal, TypeReal) -> do
                    ins line $ Exec.IMulF outvar varx outvar
                    return ()
                (Syn.MultOpMult, _, _) ->
                    cerror opstart $ "Cannot multiply values of type " ++ show typ ++ " and " ++ show typx

                (Syn.MultOpDiv, TypeInt, TypeInt) -> do
                    ins line $ Exec.IDivI outvar varx outvar
                    return ()
                (Syn.MultOpDiv, TypeReal, TypeReal) -> do
                    ins line $ Exec.IDivF outvar varx outvar
                    return ()
                (Syn.MultOpDiv, _, _) ->
                    cerror opstart $ "Cannot divide values of type " ++ show typ ++ " and " ++ show typx

                (Syn.MultOpAnd, TypeBool, TypeBool) -> do
                    ins line (Exec.IAnd outvar varx outvar)
                    return ()
                (Syn.MultOpAnd, _, _) ->
                    cerror opstart $ "Cannot apply AND operator to values of type " ++ show typ ++ " and " ++ show typx)

        return (outvar, typ)

compileSummation :: Syn.NSummation -> Compiler (String, Type)
compileSummation (Syn.Node (Syn.Summation muln oppairs) start _) = do
    let line = Syn.cursorLine start
    (var1, typ) <- compileMultiplication muln

    if null oppairs
    then
        return (var1, typ)
    else do
        outvar <- tmpvar line $ defaultOf typ
        ins line (Exec.IMov var1 outvar)
        forM_ oppairs (\(nop, mul) -> do
            let (Syn.Node op opstart _) = nop
            (varx, typx) <- compileMultiplication mul
            case (op, typ, typx) of
                (Syn.SumPlus, TypeInt, TypeInt) -> do
                    ins line $ Exec.IAddI outvar varx outvar
                    return ()
                (Syn.SumPlus, TypeReal, TypeReal) -> do
                    ins line $ Exec.IAddF outvar varx outvar
                    return ()
                (Syn.SumPlus, _, _) ->
                    cerror opstart $ "Cannot add values of type " ++ show typ ++ " and " ++ show typx

                (Syn.SumMinus, TypeInt, TypeInt) -> do
                    ins line $ Exec.ISubI outvar varx outvar
                    return ()
                (Syn.SumMinus, TypeReal, TypeReal) -> do
                    ins line $ Exec.ISubF outvar varx outvar
                    return ()
                (Syn.SumMinus, _, _) ->
                    cerror opstart $ "Cannot subtract values of type " ++ show typ ++ " and " ++ show typx

                (Syn.SumOr, TypeBool, TypeBool) -> do
                    ins line (Exec.IOr outvar varx outvar)
                    return ()
                (Syn.SumOr, _, _) ->
                    cerror opstart $ "Cannot apply OR operator to values of type " ++ show typ ++ " and " ++ show typx)

        return (outvar, typ)

compileLogOp :: (String, Type) -> Syn.NLogOperation -> (String, Type) -> Compiler String
compileLogOp (var1, typ1) nop (var2, typ2) = do
    let line = Syn.cursorLine $ Syn.nodeStart nop
    let (Syn.Node op opstart _) = nop
    outvar <- tmpvar line $ defaultOf TypeBool
    case (op, typ1, typ2) of
        (Syn.LogNeq, TypeInt, TypeInt) -> do
            ins line $ Exec.INeqI var1 var2 outvar
            return ()
        (Syn.LogNeq, TypeReal, TypeReal) -> do
            ins line $ Exec.INeqF var1 var2 outvar
            return ()
        (Syn.LogNeq, TypeBool, TypeBool) -> do
            ins line $ Exec.INeqB var1 var2 outvar
            return ()

        (Syn.LogEq, TypeInt, TypeInt) -> do
            ins line $ Exec.IEqI var1 var2 outvar
            return ()
        (Syn.LogEq, TypeReal, TypeReal) -> do
            ins line $ Exec.IEqF var1 var2 outvar
            return ()
        (Syn.LogEq, TypeBool, TypeBool) -> do
            ins line $ Exec.IEqB var1 var2 outvar
            return ()

        (Syn.LogLt, TypeInt, TypeInt) -> do
            ins line $ Exec.ILtI var1 var2 outvar
            return ()
        (Syn.LogLt, TypeReal, TypeReal) -> do
            ins line $ Exec.ILtF var1 var2 outvar
            return ()
        (Syn.LogLt, TypeBool, TypeBool) -> do
            ins line $ Exec.ILtB var1 var2 outvar
            return ()

        (Syn.LogLte, TypeInt, TypeInt) -> do
            ins line $ Exec.ILteI var1 var2 outvar
            return ()
        (Syn.LogLte, TypeReal, TypeReal) -> do
            ins line $ Exec.ILteF var1 var2 outvar
            return ()
        (Syn.LogLte, TypeBool, TypeBool) -> do
            ins line $ Exec.ILteB var1 var2 outvar
            return ()

        (Syn.LogGt, TypeInt, TypeInt) -> do
            ins line $ Exec.IGtI var1 var2 outvar
            return ()
        (Syn.LogGt, TypeReal, TypeReal) -> do
            ins line $ Exec.IGtF var1 var2 outvar
            return ()
        (Syn.LogGt, TypeBool, TypeBool) -> do
            ins line $ Exec.IGtB var1 var2 outvar
            return ()

        (Syn.LogGte, TypeInt, TypeInt) -> do
            ins line $ Exec.IGteI var1 var2 outvar
            return ()
        (Syn.LogGte, TypeReal, TypeReal) -> do
            ins line $ Exec.IGteF var1 var2 outvar
            return ()
        (Syn.LogGte, TypeBool, TypeBool) -> do
            ins line $ Exec.IGteB var1 var2 outvar
            return ()

        (_, _, _) ->
            cerror opstart $ "Cannot compare values of type " ++ show typ1 ++ " and " ++ show typ2

    return outvar

compileExpression :: Syn.NExpression -> Compiler (String, Type)
compileExpression (Syn.Node (Syn.Expression sumn oppairs) start _) = do
    let line = Syn.cursorLine start
    (var1, typ1) <- compileSummation sumn
    case oppairs of
        [] ->
            return (var1, typ1)
        [(nopx, sumnx)] -> do
            (var2, typ2) <- compileSummation sumnx
            outvar <- compileLogOp (var1, typ1) nopx (var2, typ2)
            return (outvar, TypeBool)
        _ -> do
            (logPairs, _, _) <- foldM (\(pairs, lastvar, lasttyp) (nopx, sumnx) -> do
                (varx, typx) <- compileSummation sumnx
                let newPairs = ((lastvar, lasttyp), nopx, (varx, typx)):pairs
                return (newPairs, varx, typx)) ([], var1, typ1) oppairs

            pairVars <- mapM (\(vt1, op, vt2) -> compileLogOp vt1 op vt2) logPairs

            outvar <- tmpvar line $ Exec.ValBool True
            forM_ pairVars (\var -> ins line $ Exec.IAnd outvar var outvar)

            return (outvar, TypeBool)

compileStatement :: Syn.NStatement -> Compiler Int
compileStatement (Syn.Node (Syn.StmtCompound stmts) start _) = do
    let line = Syn.cursorLine start
    case stmts of
        [] ->
            ins line Exec.INop
        s:ss -> do
            ind <- compileStatement s
            forM_ ss compileStatement
            return ind

compileStatement (Syn.Node (Syn.StmtAssignment ident expr) start _) = do
    let line = Syn.cursorLine start
    let vvar = Syn.nodeValue ident
    vtyp <- gettype vvar (Syn.nodeStart ident)
    ind <- ins line Exec.INop
    (evar, etyp) <- compileExpression expr
    when (etyp /= vtyp) $
        cerror start $ "Cannot assign value of typ " ++ show etyp ++ " to a variable of type " ++ show vtyp
    ins line $ Exec.IMov evar vvar
    return ind

compileStatement (Syn.Node (Syn.StmtCondition cond thenB mElseB) start _) = do
    let line = Syn.cursorLine start
    firstInd <- ins line Exec.INop
    (cvar, ctype) <- compileExpression cond
    when (ctype /= TypeBool) $
        cerror start "IF operator's condition must be of type Bool"

    notCvar <- tmpvar line (defaultOf TypeBool)
    ins line $ Exec.INot cvar notCvar

    jmpToElseInd <- ins line $ Exec.IJmpC notCvar 0

    compileStatement thenB
    jmpToEndInd <- ins line $ Exec.IJmp 0

    case mElseB of
        Nothing -> do
            endInd <- ins line Exec.INop
            setins jmpToElseInd line $ Exec.IJmpC notCvar endInd
            setins jmpToEndInd line Exec.INop
            return firstInd
        Just elseB -> do
            elseInd <- compileStatement elseB
            endInd <- ins line Exec.INop
            setins jmpToElseInd line $ Exec.IJmpC notCvar elseInd
            setins jmpToEndInd line $ Exec.IJmp endInd
            return firstInd

compileStatement (Syn.Node (Syn.StmtForLoop ident initv finv body) start _) = do
    let line = Syn.cursorLine start
    let paramVar = Syn.nodeValue ident
    typ <- gettype paramVar (Syn.nodeStart ident)
    when (typ /= TypeInt) $
        cerror (Syn.nodeStart ident) "FOR loop's parameter must be of integer type"

    firstInd <- ins line Exec.INop

    (initVar, initTyp) <- compileExpression initv
    when (initTyp /= TypeInt) $
        cerror (Syn.nodeStart ident) "FOR loop's initial value must be of integer type"

    (finVar, finTyp) <- compileExpression finv
    when (finTyp /= TypeInt) $
        cerror (Syn.nodeStart ident) "FOR loop's final value must be of integer type"

    -- Initial assignment
    ins line $ Exec.IMov initVar paramVar

    -- Increment step
    incStep <- tmpvar line $ Exec.ValInt 1

    -- Condition check
    condVar <- tmpvar line $ defaultOf TypeBool
    bodyStartInd <- ins line $ Exec.IGteI paramVar finVar condVar
    jmpEndInd <- ins line $ Exec.IJmpC condVar 0

    compileStatement body
    ins line $ Exec.IAddI paramVar incStep paramVar
    ins line $ Exec.IJmp bodyStartInd

    endInd <- ins line Exec.INop
    setins jmpEndInd line $ Exec.IJmpC condVar endInd

    return firstInd

compileStatement (Syn.Node (Syn.StmtWhileLoop cond body) start _) = do
    let line = Syn.cursorLine start
    startInd <- ins line Exec.INop
    (condVar, condTyp) <- compileExpression cond
    when (condTyp /= TypeBool) $
        cerror start "WHILE loops's condition must of type Bool"

    notCondVar <- tmpvar line $ defaultOf TypeBool
    ins line $ Exec.INot condVar notCondVar
    jmpInd <- ins line $ Exec.IJmpC notCondVar 0

    compileStatement body
    ins line $ Exec.IJmp startInd
    endInd <- ins line Exec.INop

    setins jmpInd line $ Exec.IJmpC notCondVar endInd

    return startInd

compileStatement (Syn.Node (Syn.StmtRead idents) start _) = do
    let line = Syn.cursorLine start
    startInd <- ins line Exec.INop
    forM_ idents (\(Syn.Node ident istart _) -> do
        typ <- gettype ident istart
        case typ of
            TypeInt  -> ins line $ Exec.IReadI ident
            TypeReal -> ins line $ Exec.IReadF ident
            TypeBool -> ins line $ Exec.IReadB ident
        return ())
    return startInd

compileStatement (Syn.Node (Syn.StmtWrite exprs) start _) = do
    let line = Syn.cursorLine start
    startInd <- ins line Exec.INop
    forM_ exprs (\expr -> do
        (var, typ) <- compileExpression expr
        case typ of
            TypeInt  -> ins line $ Exec.IWriteI var
            TypeReal -> ins line $ Exec.IWriteF var
            TypeBool -> ins line $ Exec.IWriteB var
        return ())
    return startInd

compileBlock :: Syn.NBlock -> Compiler ()
compileBlock (Syn.Node (Syn.BlockDecl idents styp) start _) = do
    let line = Syn.cursorLine start
    let typ = fromSynType $ Syn.nodeValue styp
    forM_ idents (\(Syn.Node ident start _) -> do
        isdef <- isdefined ident
        when isdef $
            cerror start $ "Variable " ++ show ident ++ " is already defined"
        _ <- ins line $ Exec.IDecl ident (defaultOf typ)
        settype ident typ
        return ())
compileBlock (Syn.Node (Syn.BlockStmt stmt) _ _) = do
    _ <- compileStatement stmt
    return ()

compileProgram :: Syn.NProgram -> Compiler ()
compileProgram (Syn.Node (Syn.Program blocks) _ _) =
    forM_ blocks compileBlock

compile :: Syn.NProgram -> Either Error (Table (Int, Exec.Instruction))
compile prg =
    case runCompiler (compileProgram prg) newState of
        Left err      -> Left err
        Right (_, st) -> Right $ stInstr st
