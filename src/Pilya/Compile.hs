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
    { stInstr     :: Table Exec.Instruction
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

ins :: Exec.Instruction -> Compiler Int
ins i = Compiler (\(State is cnt tm) ->
    let (ind, is2) = Tbl.append is i in
    Right (ind, State is2 cnt tm))

setins :: Int -> Exec.Instruction -> Compiler ()
setins ind ins = Compiler (\(State is cnt tm) ->
    Right ((), State (Tbl.set is ind ins) cnt tm))

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

tmpvar :: Exec.Value -> Compiler String
tmpvar val = do
    tmpcnt <- tmpcnt
    let varname = "$t" ++ show (tmpcnt + 1)
    ins $ Exec.IDecl varname val
    inctmpcnt
    settype varname (typeOf val)
    return varname

compileMultiplier :: Syn.NMultiplier -> Compiler (String, Type)
compileMultiplier (Syn.Node (Syn.MultIdent ident) start _) = do
    tp <- gettype (Syn.nodeValue ident) (Syn.nodeStart ident)
    return (Syn.nodeValue ident, tp)
compileMultiplier (Syn.Node (Syn.MultNumber num) start _) =
    case Syn.nodeValue num of
        Syn.NumInteger int -> do
            varname <- tmpvar (Exec.ValInt int)
            return (varname, TypeInt)
        Syn.NumReal dbl -> do
            varname <- tmpvar (Exec.ValReal dbl)
            return (varname, TypeReal)
compileMultiplier (Syn.Node (Syn.MultBool bool) start _) = do
    let b = case Syn.nodeValue bool of
            Syn.BoolTrue  -> True
            Syn.BoolFalse -> False
    varname <- tmpvar (Exec.ValBool b)
    return (varname, TypeBool)
compileMultiplier (Syn.Node (Syn.MultNot mulr) start _) = do
    (varname, tp) <- compileMultiplier mulr
    when (tp /= TypeBool) $
        cerror start $ "Can't apply 'not' operator to a value of type " ++ show tp
    outname <- tmpvar (defaultOf TypeBool)
    ins (Exec.INot varname outname)
    return (outname, TypeBool)
compileMultiplier (Syn.Node (Syn.MultGrouped expr) _ _) =
    compileExpression expr

compileMultiplication :: Syn.NMultiplication -> Compiler (String, Type)
compileMultiplication (Syn.Node (Syn.Multiplication mulr oppairs) _ _) = do
    (var1, typ) <- compileMultiplier mulr

    if null oppairs
    then
        return (var1, typ)
    else do
        outvar <- tmpvar $ defaultOf typ
        ins (Exec.IMov var1 outvar)
        forM_ oppairs (\(nop, mul) -> do
            let (Syn.Node op opstart _) = nop
            (varx, typx) <- compileMultiplier mul
            case (op, typ, typx) of
                (Syn.MultOpMult, TypeInt, TypeInt) -> do
                    ins $ Exec.IMulI outvar varx outvar
                    return ()
                (Syn.MultOpMult, TypeReal, TypeReal) -> do
                    ins $ Exec.IMulF outvar varx outvar
                    return ()
                (Syn.MultOpMult, _, _) ->
                    cerror opstart $ "Cannot multiply values of type " ++ show typ ++ " and " ++ show typx

                (Syn.MultOpDiv, TypeInt, TypeInt) -> do
                    ins $ Exec.IDivI outvar varx outvar
                    return ()
                (Syn.MultOpDiv, TypeReal, TypeReal) -> do
                    ins $ Exec.IDivF outvar varx outvar
                    return ()
                (Syn.MultOpDiv, _, _) ->
                    cerror opstart $ "Cannot divide values of type " ++ show typ ++ " and " ++ show typx

                (Syn.MultOpAnd, TypeBool, TypeBool) -> do
                    ins (Exec.IAnd outvar varx outvar)
                    return ()
                (Syn.MultOpAnd, _, _) ->
                    cerror opstart $ "Cannot apply AND operator to values of type " ++ show typ ++ " and " ++ show typx)

        return (outvar, typ)

compileSummation :: Syn.NSummation -> Compiler (String, Type)
compileSummation (Syn.Node (Syn.Summation muln oppairs) _ _) = do
    (var1, typ) <- compileMultiplication muln

    if null oppairs
    then
        return (var1, typ)
    else do
        outvar <- tmpvar $ defaultOf typ
        ins (Exec.IMov var1 outvar)
        forM_ oppairs (\(nop, mul) -> do
            let (Syn.Node op opstart _) = nop
            (varx, typx) <- compileMultiplication mul
            case (op, typ, typx) of
                (Syn.SumPlus, TypeInt, TypeInt) -> do
                    ins $ Exec.IAddI outvar varx outvar
                    return ()
                (Syn.SumPlus, TypeReal, TypeReal) -> do
                    ins $ Exec.IAddF outvar varx outvar
                    return ()
                (Syn.SumPlus, _, _) ->
                    cerror opstart $ "Cannot add values of type " ++ show typ ++ " and " ++ show typx

                (Syn.SumMinus, TypeInt, TypeInt) -> do
                    ins $ Exec.ISubI outvar varx outvar
                    return ()
                (Syn.SumMinus, TypeReal, TypeReal) -> do
                    ins $ Exec.ISubF outvar varx outvar
                    return ()
                (Syn.SumMinus, _, _) ->
                    cerror opstart $ "Cannot subtract values of type " ++ show typ ++ " and " ++ show typx

                (Syn.SumOr, TypeBool, TypeBool) -> do
                    ins (Exec.IOr outvar varx outvar)
                    return ()
                (Syn.SumOr, _, _) ->
                    cerror opstart $ "Cannot apply OR operator to values of type " ++ show typ ++ " and " ++ show typx)

        return (outvar, typ)

compileLogOp :: (String, Type) -> Syn.NLogOperation -> (String, Type) -> Compiler String
compileLogOp (var1, typ1) nop (var2, typ2) = do
    let (Syn.Node op opstart _) = nop
    outvar <- tmpvar $ defaultOf TypeBool
    case (op, typ1, typ2) of
        (Syn.LogNeq, TypeInt, TypeInt) -> do
            ins $ Exec.INeqI var1 var2 outvar
            return ()
        (Syn.LogNeq, TypeReal, TypeReal) -> do
            ins $ Exec.INeqF var1 var2 outvar
            return ()
        (Syn.LogNeq, TypeBool, TypeBool) -> do
            ins $ Exec.INeqB var1 var2 outvar
            return ()

        (Syn.LogEq, TypeInt, TypeInt) -> do
            ins $ Exec.IEqI var1 var2 outvar
            return ()
        (Syn.LogEq, TypeReal, TypeReal) -> do
            ins $ Exec.IEqF var1 var2 outvar
            return ()
        (Syn.LogEq, TypeBool, TypeBool) -> do
            ins $ Exec.IEqB var1 var2 outvar
            return ()

        (Syn.LogLt, TypeInt, TypeInt) -> do
            ins $ Exec.ILtI var1 var2 outvar
            return ()
        (Syn.LogLt, TypeReal, TypeReal) -> do
            ins $ Exec.ILtF var1 var2 outvar
            return ()
        (Syn.LogLt, TypeBool, TypeBool) -> do
            ins $ Exec.ILtB var1 var2 outvar
            return ()

        (Syn.LogLte, TypeInt, TypeInt) -> do
            ins $ Exec.ILteI var1 var2 outvar
            return ()
        (Syn.LogLte, TypeReal, TypeReal) -> do
            ins $ Exec.ILteF var1 var2 outvar
            return ()
        (Syn.LogLte, TypeBool, TypeBool) -> do
            ins $ Exec.ILteB var1 var2 outvar
            return ()

        (Syn.LogGt, TypeInt, TypeInt) -> do
            ins $ Exec.IGtI var1 var2 outvar
            return ()
        (Syn.LogGt, TypeReal, TypeReal) -> do
            ins $ Exec.IGtF var1 var2 outvar
            return ()
        (Syn.LogGt, TypeBool, TypeBool) -> do
            ins $ Exec.IGtB var1 var2 outvar
            return ()

        (Syn.LogGte, TypeInt, TypeInt) -> do
            ins $ Exec.IGteI var1 var2 outvar
            return ()
        (Syn.LogGte, TypeReal, TypeReal) -> do
            ins $ Exec.IGteF var1 var2 outvar
            return ()
        (Syn.LogGte, TypeBool, TypeBool) -> do
            ins $ Exec.IGteB var1 var2 outvar
            return ()

        (_, _, _) ->
            cerror opstart $ "Cannot compare values of type " ++ show typ1 ++ " and " ++ show typ2

    return outvar

compileExpression :: Syn.NExpression -> Compiler (String, Type)
compileExpression (Syn.Node (Syn.Expression sumn oppairs) _ _) = do
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

            outvar <- tmpvar $ Exec.ValBool True
            forM_ pairVars (\var -> ins $ Exec.IAnd outvar var outvar)

            return (outvar, TypeBool)

compileStatement :: Syn.NStatement -> Compiler Int
compileStatement (Syn.Node (Syn.StmtCompound stmts) _ _) =
    case stmts of
        [] ->
            ins Exec.INop
        s:ss -> do
            ind <- compileStatement s
            forM_ ss compileStatement
            return ind

compileStatement (Syn.Node (Syn.StmtAssignment ident expr) start _) = do
    let vvar = Syn.nodeValue ident
    vtyp <- gettype vvar (Syn.nodeStart ident)
    ind <- ins Exec.INop
    (evar, etyp) <- compileExpression expr
    when (etyp /= vtyp) $
        cerror start $ "Cannot assign value of typ " ++ show etyp ++ " to a variable of type " ++ show vtyp
    ins $ Exec.IMov evar vvar
    return ind

compileStatement (Syn.Node (Syn.StmtCondition cond thenB mElseB) start _) = do
    firstInd <- ins Exec.INop
    (cvar, ctype) <- compileExpression cond
    when (ctype /= TypeBool) $
        cerror start "IF operator's condition must be of type Bool"

    notCvar <- tmpvar (defaultOf TypeBool)
    ins $ Exec.INot cvar notCvar

    jmpToElseInd <- ins $ Exec.IJmpC notCvar 0

    compileStatement thenB
    jmpToEndInd <- ins $ Exec.IJmp 0

    case mElseB of
        Nothing -> do
            endInd <- ins Exec.INop
            setins jmpToElseInd $ Exec.IJmpC notCvar endInd
            setins jmpToEndInd Exec.INop
            return firstInd
        Just elseB -> do
            elseInd <- compileStatement elseB
            endInd <- ins Exec.INop
            setins jmpToElseInd $ Exec.IJmpC notCvar elseInd
            setins jmpToEndInd $ Exec.IJmp endInd
            return firstInd

compileStatement (Syn.Node (Syn.StmtForLoop ident initv finv body) _ _) = do
    let paramVar = Syn.nodeValue ident
    typ <- gettype paramVar (Syn.nodeStart ident)
    when (typ /= TypeInt) $
        cerror (Syn.nodeStart ident) "FOR loop's parameter must be of integer type"

    firstInd <- ins Exec.INop

    (initVar, initTyp) <- compileExpression initv
    when (initTyp /= TypeInt) $
        cerror (Syn.nodeStart ident) "FOR loop's initial value must be of integer type"

    (finVar, finTyp) <- compileExpression finv
    when (finTyp /= TypeInt) $
        cerror (Syn.nodeStart ident) "FOR loop's final value must be of integer type"

    -- Initial assignment
    ins $ Exec.IMov initVar paramVar

    -- Increment step
    incStep <- tmpvar $ Exec.ValInt 1

    -- Condition check
    condVar <- tmpvar $ defaultOf TypeBool
    bodyStartInd <- ins $ Exec.IGteI paramVar finVar condVar
    jmpEndInd <- ins $ Exec.IJmpC condVar 0

    compileStatement body
    ins $ Exec.IAddI paramVar incStep paramVar
    ins $ Exec.IJmp bodyStartInd

    endInd <- ins Exec.INop
    setins jmpEndInd $ Exec.IJmpC condVar endInd

    return firstInd

compileStatement (Syn.Node (Syn.StmtWhileLoop cond body) start _) = do
    startInd <- ins Exec.INop
    (condVar, condTyp) <- compileExpression cond
    when (condTyp /= TypeBool) $
        cerror start "WHILE loops's condition must of type Bool"

    notCondVar <- tmpvar $ defaultOf TypeBool
    ins $ Exec.INot condVar notCondVar
    jmpInd <- ins $ Exec.IJmpC notCondVar 0

    compileStatement body
    ins $ Exec.IJmp startInd
    endInd <- ins Exec.INop

    setins jmpInd $ Exec.IJmpC notCondVar endInd

    return startInd

compileStatement (Syn.Node (Syn.StmtRead idents) _ _) = do
    startInd <- ins Exec.INop
    forM_ idents (\(Syn.Node ident istart _) -> do
        typ <- gettype ident istart
        case typ of
            TypeInt  -> ins $ Exec.IReadI ident
            TypeReal -> ins $ Exec.IReadF ident
            TypeBool -> ins $ Exec.IReadB ident
        return ())
    return startInd

compileStatement (Syn.Node (Syn.StmtWrite exprs) _ _) = do
    startInd <- ins Exec.INop
    forM_ exprs (\expr -> do
        (var, typ) <- compileExpression expr
        case typ of
            TypeInt  -> ins $ Exec.IWriteI var
            TypeReal -> ins $ Exec.IWriteF var
            TypeBool -> ins $ Exec.IWriteB var
        return ())
    return startInd

compileBlock :: Syn.NBlock -> Compiler ()
compileBlock (Syn.Node (Syn.BlockDecl idents styp) _ _) = do
    let typ = fromSynType $ Syn.nodeValue styp
    forM_ idents (\(Syn.Node ident start _) -> do
        isdef <- isdefined ident
        when isdef $
            cerror start $ "Variable " ++ show ident ++ " is already defined"
        _ <- ins $ Exec.IDecl ident (defaultOf typ)
        settype ident typ
        return ())
compileBlock (Syn.Node (Syn.BlockStmt stmt) _ _) = do
    _ <- compileStatement stmt
    return ()

compileProgram :: Syn.NProgram -> Compiler ()
compileProgram (Syn.Node (Syn.Program blocks) _ _) =
    forM_ blocks compileBlock

compile :: Syn.NProgram -> Either Error (Table Exec.Instruction)
compile prg =
    case runCompiler (compileProgram prg) newState of
        Left err      -> Left err
        Right (_, st) -> Right $ stInstr st
