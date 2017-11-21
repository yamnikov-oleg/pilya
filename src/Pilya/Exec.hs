module Pilya.Exec
    ( Value(..)
    , VarMap
    , CloseStatus (..)
    , Input (..)
    , Env (..)
    , emptyEnv
    , envCloseInput
    , envPushInput
    , Instruction (..)
    , ExecResult (..)
    , exec
    ) where

import qualified Data.Map as M

data Value
    = ValBool Bool
    | ValInt Integer
    | ValReal Double
    deriving (Show)

type VarMap = M.Map String Value

-- CloseStatus defines whether the interpreter can request the calling code
-- for more user input. This value should stay Open until the caller reach
-- EOF while reading input file.
data CloseStatus = Closed | Open

data Input = Input String CloseStatus

data Env = Env VarMap Input

emptyEnv :: Env
emptyEnv = Env M.empty (Input "" Open)

envCloseInput :: Env -> Env
envCloseInput (Env vm (Input buf _)) = Env vm (Input buf Closed)

envPushInput :: Env -> String -> Env
envPushInput = undefined

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
    | INeqI String String String
    | IEqI String String String
    | ILtI String String String
    | ILteI String String String
    | IGtI String String String
    | IGteI String String String

    | INeqF String String String
    | IEqF String String String
    | ILtF String String String
    | ILteF String String String
    | IGtF String String String
    | IGteF String String String

    | INeqB String String String
    | IEqB String String String
    | ILtB String String String
    | ILteB String String String
    | IGtB String String String
    | IGteB String String String

    | IAddI String String String
    | ISubI String String String
    | IAddF String String String
    | ISubF String String String
    | IOr String String String

    | IMulI String String String
    | IDivI String String String
    | IMulF String String String
    | IDivF String String String
    | IAnd String String String
    -- Unary instruction. Operand order: src dst
    | INot String String
    -- Type conversion. Operands: src dst
    | IFloat String String
    | IRound String String
    -- Unconditional jump to instruction by index.
    | IJmp Int
    -- Conditional jump. If variable is True, jump to instruction by index.
    | IJmpC String Int
    -- Read value from input to a variable
    | IReadI String
    | IReadF String
    | IReadB String
    -- Write variable value to the ouput
    | IWriteI String
    | IWriteF String
    | IWriteB String
    deriving (Show)

data ExecResult
    -- Instruction has been executed successfully. The first value is
    -- the updated environment. The second value is optional output.
    -- The third value is optional jump address.
    = ExecOk Env (Maybe String) (Maybe Int)
    -- If the input's close status is `Open`, the interpreter will return this
    -- command to ask for more data in the input buffer.
    -- If the input is closed, the interpreter will raise an error instead.
    | ExecRequestInput
    -- An error occured while executing the instruction.
    | ExecError String

exec :: Instruction -> Env -> ExecResult
exec = undefined
