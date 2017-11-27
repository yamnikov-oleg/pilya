module Pilya.Exec
    ( Value(..)
    , VarMap
    , CloseStatus (..)
    , Input (..)
    , Env (..)
    , Instruction (..)
    , exec
    , execNoLoops
    , run'
    , run
    ) where

import           Control.Monad (foldM, when)
import qualified Data.Map      as M
import qualified Pilya.Table   as Tbl
import           Text.Read     (readMaybe)

data Value
    = ValBool Bool
    | ValInt Integer
    | ValReal Double
    deriving (Show, Eq, Ord)

add :: Value -> Value -> Either String Value
add (ValInt i1) (ValInt i2)   = Right $ ValInt (i1 + i2)
add (ValReal d1) (ValReal d2) = Right $ ValReal (d1 + d2)
add _ _                       = Left "mismatched types on addition"

sub :: Value -> Value -> Either String Value
sub (ValInt i1) (ValInt i2)   = Right $ ValInt (i1 - i2)
sub (ValReal d1) (ValReal d2) = Right $ ValReal (d1 - d2)
sub _ _                       = Left "mismatched types on substraction"

mult :: Value -> Value -> Either String Value
mult (ValInt i1) (ValInt i2)   = Right $ ValInt (i1 * i2)
mult (ValReal d1) (ValReal d2) = Right $ ValReal (d1 * d2)
mult _ _                       = Left "mismatched types on multiplication"

divv :: Value -> Value -> Either String Value
divv (ValInt i1) (ValInt i2)   = Right $ ValInt (i1 `div` i2)
divv (ValReal d1) (ValReal d2) = Right $ ValReal (d1 / d2)
divv _ _                       = Left "mismatched types on division"

type VarMap = M.Map String Value

-- CloseStatus defines whether the interpreter can request the calling code
-- for more user input. This value should stay Open until the caller reach
-- EOF while reading input file.
data CloseStatus = Closed | Open

data Input = Input String CloseStatus

data Env = Env Int VarMap [String] [String]

envOutput :: Env -> [String]
envOutput (Env _ _ _ o) = o

data Instruction
    -- No op
    = INop
    -- Declare variable with initial value
    | IDecl String Value
    -- Delete the variable from environment
    | IFree String
    -- Move variable's value into another variable. Operand order: src dst
    | IMov String String
    -- Binary instructions. Operand order: src1 src2 dst
    -- Both operand type must be equal.
    -- 'I' suffix denotes integer type, 'F' - real type, 'B' - bool type.
    | INeq String String String
    | IEq String String String
    | ILt String String String
    | ILte String String String
    | IGt String String String
    | IGte String String String

    | IAdd String String String
    | ISub String String String
    | IOr String String String

    | IMul String String String
    | IDiv String String String
    | IAnd String String String
    -- Unary instruction. Operand order: src dst
    | INot String String
    -- Type conversion. Operands: src dst
    | IFloat String String
    | ITrunc String String
    -- Unconditional jump to instruction by index.
    | IJmp Int
    -- Conditional jump. If variable is True, jump to instruction by index.
    | IJmpC String Int
    -- Read value from input to a variable
    | IReadI String
    | IReadF String
    | IReadB String
    -- Write variable value to the ouput
    | IWrite String
    deriving (Show)

incii :: Env -> Env
incii (Env ind vars inp outp) = Env (ind+1) vars inp outp

require :: Env -> String -> Either (Int, String) Value
require (Env ind vars _ _) name =
    case M.lookup name vars of
        Nothing  -> Left (ind, "var " ++ name ++ " undefined")
        Just val -> Right val

requireBool :: Env -> String -> Either (Int, String) Bool
requireBool env@(Env ind _ _ _) name = do
    val <- require env name
    case val of
        ValBool b -> Right b
        _         -> Left (ind, "value should be bool")

requireInt :: Env -> String -> Either (Int, String) Integer
requireInt env@(Env ind _ _ _) name = do
    val <- require env name
    case val of
        ValInt i -> Right i
        _        -> Left (ind, "value should be int")

requireReal :: Env -> String -> Either (Int, String) Double
requireReal env@(Env ind _ _ _) name = do
    val <- require env name
    case val of
        ValReal d -> Right d
        _         -> Left (ind, "value should be real")

errii :: Int -> Either String a -> Either (Int, String) a
errii ind (Left s) = Left (ind, s)
errii _ (Right x)  = Right x

execCmp :: Env -> String -> String -> (Value -> Value -> Bool) -> String -> Either (Int, String) Env
execCmp env@(Env ind vars inp outp) src1 src2 f dst = do
    val1 <- require env src1
    val2 <- require env src2
    let res = f val1 val2
    return $ incii $ Env ind (M.insert dst (ValBool res) vars) inp outp

execNumOp :: Env -> String -> String -> (Value -> Value -> Either String Value) -> String -> Either (Int, String) Env
execNumOp env@(Env ind vars inp outp) src1 src2 op dst = do
    val1 <- require env src1
    val2 <- require env src2
    res <- errii ind $ op val1 val2
    return $ incii $ Env ind (M.insert dst res vars) inp outp

execRead :: Read a => Env -> String -> (a -> Value) -> Either (Int, String) Env
execRead (Env ind _    []         _   ) _   _ = Left (ind, "unexpected EOF")
execRead (Env ind vars (inp:inps) outp) dst t =
    case readMaybe inp of
        Nothing ->
            Left (ind, "read failed")
        Just i ->
            Right $ incii $ Env ind (M.insert dst (t i) vars) inps outp

exec :: Env -> Instruction -> Either (Int, String) Env
exec env INop =
    Right $ incii env
exec (Env ind vars inp outp) (IDecl name val) =
    Right $ incii $ Env ind (M.insert name val vars) inp outp
exec (Env ind vars inp outp) (IFree name) =
    Right $ incii $ Env ind (M.delete name vars) inp outp
exec env@(Env ind vars inp outp) (IMov src dst) = do
    val <- require env src
    return $ incii $ Env ind (M.insert dst val vars) inp outp

exec env (INeq src1 src2 dst) = execCmp env src1 src2 (/=) dst
exec env (IEq src1 src2 dst) = execCmp env src1 src2 (==) dst
exec env (ILt src1 src2 dst) = execCmp env src1 src2 (<) dst
exec env (ILte src1 src2 dst) = execCmp env src1 src2 (<=) dst
exec env (IGt src1 src2 dst) = execCmp env src1 src2 (>) dst
exec env (IGte src1 src2 dst) = execCmp env src1 src2 (>=) dst

exec env (IAdd src1 src2 dst) = execNumOp env src1 src2 add dst
exec env (ISub src1 src2 dst) = execNumOp env src1 src2 sub dst
exec env (IMul src1 src2 dst) = execNumOp env src1 src2 mult dst
exec env (IDiv src1 src2 dst) = execNumOp env src1 src2 divv dst

exec env@(Env ind vars inp outp) (IAnd src1 src2 dst) = do
    b1 <- requireBool env src1
    b2 <- requireBool env src2
    return $ incii $ Env ind (M.insert dst (ValBool (b1 && b2)) vars) inp outp
exec env@(Env ind vars inp outp) (IAnd src1 src2 dst) = do
    b1 <- requireBool env src1
    b2 <- requireBool env src2
    return $ incii $ Env ind (M.insert dst (ValBool (b1 || b2)) vars) inp outp
exec env@(Env ind vars inp outp) (INot src dst) = do
    b <- requireBool env src
    return $ incii $ Env ind (M.insert dst (ValBool (not b)) vars) inp outp

exec env@(Env ind vars inp outp) (IFloat src dst) = do
    i <- requireInt env src
    return $ incii $ Env ind (M.insert dst (ValReal (fromIntegral i)) vars) inp outp
exec env@(Env ind vars inp outp) (ITrunc src dst) = do
    d <- requireReal env src
    return $ incii $ Env ind (M.insert dst (ValInt (truncate d)) vars) inp outp

exec (Env _ vars inp outp) (IJmp newInd) =
    Right $ Env newInd vars inp outp
exec env@(Env ind vars inp outp) (IJmpC cond newInd) = do
    b <- requireBool env cond
    if b
        then Right $ Env newInd vars inp outp
        else Right $ Env (ind+1) vars inp outp

exec env (IReadI dst) = execRead env dst ValInt
exec env (IReadF dst) = execRead env dst ValReal
exec env (IReadB dst) = execRead env dst ValBool

exec env@(Env ind vars inp outp) (IWrite src) = do
    val <- require env src
    case val of
        ValInt i  -> Right $ incii $ Env ind vars inp (show i:outp)
        ValReal d -> Right $ incii $ Env ind vars inp (show d:outp)
        ValBool b -> Right $ incii $ Env ind vars inp (show b:outp)

execNoLoops :: Env -> Instruction -> Either (Int, String) Env
execNoLoops env@(Env ind _ _ _) ins = do
    env2@(Env ind2 _ _ _) <- exec env ins
    when (ind == ind2) $ Left (ind, "loop")
    return env2

run' :: Env -> Integer -> Tbl.Table Instruction -> Either (Int, String) Env
run' (Env ind _ _ _) 0 _ = Left (ind, "exceeded number of operations")
run' env@(Env ind _ _ _) opc tbl =
    case Tbl.tlookup tbl ind of
        Nothing -> Right env
        Just ins -> do
            newEnv <- execNoLoops env ins
            run' newEnv (opc-1) tbl

maxOpsCount :: Integer
maxOpsCount = 1000*1000

run :: Tbl.Table Instruction -> [String] -> Either (Int, String) [String]
run tbl inp = envOutput <$> run' (Env 0 M.empty inp []) maxOpsCount tbl
