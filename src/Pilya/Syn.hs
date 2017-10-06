module Pilya.Syn
    ( Type (..)
    , NumberLiteral (..)
    , BoolLiteral (..)
    , Multiplier (..)
    , MultOperation (..)
    , Multiplication (..)
    , SumOperation (..)
    , Summation (..)
    , LogOperation (..)
    , Expression (..)
    , Statement (..)
    , Block (..)
    , Program (..)
    , parse
    , ParserError (..)
    )
    where

import           Control.Monad (when)
import           Pilya.Lex     (Token (..), TokenType (..))
import           Pilya.Parcomb (Parser (..), ParserError (..), consume, expect,
                                expectAny, lookahead, many1sep, parserError,
                                skip, tryParse)
import qualified Pilya.Parcomb as Parcomb

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

newlines :: Parser ()
newlines = do
    tt <- lookahead
    when (tt == TokNewline) $ do
        skip
        newlines

type Identifier = String

data Type
    = TypeInt
    | TypeReal
    | TypeBool
    deriving (Show)

typeFromToken :: TokenType -> Type
typeFromToken TokPercent = TypeInt
typeFromToken TokExcl = TypeReal
typeFromToken TokDollar = TypeBool
typeFromToken tt = error $ "Token " ++ show tt ++ " does not denote a type"

identifier :: Parser Identifier
identifier = do
    tt <- lookahead
    case tt of
        TokIdent s -> do { consume; return s }
        _          -> parserError "Expected identifier"

declaration :: Parser ([Identifier], Type)
declaration = do
    expect TokKwDim
    idents <- many1sep (expect TokComma) identifier
    typeTok <- expectAny [TokPercent, TokExcl, TokDollar]
    return (idents, typeFromToken typeTok)

data NumberLiteral
    = NumInteger Integer
    | NumReal Double
    deriving (Show)

numberLiteral :: Parser NumberLiteral
numberLiteral = do
    tt <- lookahead
    case tt of
        TokInteger int -> do
            skip
            return $ NumInteger int
        TokReal dbl -> do
            skip
            return $ NumReal dbl
        _ -> parserError $ "Expected number, got " ++ show tt

data BoolLiteral = BoolTrue | BoolFalse deriving (Show)

boolLiteral :: Parser BoolLiteral
boolLiteral = do
    tt <- lookahead
    case tt of
        TokKwTrue -> do
            skip
            return BoolTrue
        TokKwFalse -> do
            skip
            return BoolFalse
        _ -> parserError $ "Expected boolean, got " ++ show tt

data Multiplier
    = MultIdent Identifier
    | MultNumber NumberLiteral
    | MultBool BoolLiteral
    | MultNot Multiplier
    deriving (Show)

multiplier :: Parser Multiplier
multiplier = do
    tt <- lookahead
    case tt of
        TokIdent s -> do
            skip
            return $ MultIdent s
        TokInteger _ ->
            fmap MultNumber numberLiteral
        TokReal _ ->
            fmap MultNumber numberLiteral
        TokKwTrue ->
            fmap MultBool boolLiteral
        TokKwFalse ->
            fmap MultBool boolLiteral
        TokKwNot -> do
            skip
            fmap MultNot multiplier
        _ -> parserError $ "Expected multiplier, got " ++ show tt

data MultOperation
    = MultOpMult -- *
    | MultOpDiv  -- /
    | MultOpAnd  -- and
    deriving (Show)

multOperation :: Parser MultOperation
multOperation = do
    tt <- expectAny [TokMult, TokDiv, TokKwAnd]
    case tt of
        TokMult  -> return MultOpMult
        TokDiv   -> return MultOpDiv
        TokKwAnd -> return MultOpAnd

data Multiplication
    = Multiplication Multiplier [(MultOperation, Multiplier)]
    deriving (Show)

multiplication :: Parser Multiplication
multiplication = do
    m <- multiplier
    ms <- multiplication'
    return $ Multiplication m ms

multiplication' :: Parser [(MultOperation, Multiplier)]
multiplication' = do
    res <- tryParse multOperation
    case res of
        Left _ -> return []
        Right op -> do
            mult <- multiplier
            pairs <- multiplication'
            return $ (op, mult):pairs

data SumOperation
    = SumPlus
    | SumMinus
    | SumOr
    deriving (Show)

sumOperation :: Parser SumOperation
sumOperation = do
    tt <- expectAny [TokPlus, TokMinus, TokKwOr]
    case tt of
        TokPlus  -> return SumPlus
        TokMinus -> return SumMinus
        TokKwOr  -> return SumOr

data Summation
    = Summation Multiplication [(SumOperation, Multiplication)]
    deriving (Show)

summation :: Parser Summation
summation = do
    m <- multiplication
    ms <- summation'
    return $ Summation m ms

summation' :: Parser [(SumOperation, Multiplication)]
summation' = do
    res <- tryParse sumOperation
    case res of
        Left _ -> return []
        Right op -> do
            mult <- multiplication
            pairs <- summation'
            return $ (op, mult):pairs

data LogOperation
    = LogNeq
    | LogEq
    | LogLt
    | LogLte
    | LogGt
    | LogGte
    deriving (Show)

logOperation :: Parser LogOperation
logOperation = do
    tt <- expectAny [TokNeq, TokEq, TokLt, TokLte, TokGt, TokGte]
    case tt of
        TokNeq -> return LogNeq
        TokEq  -> return LogEq
        TokLt  -> return LogLt
        TokLte -> return LogLte
        TokGt  -> return LogGt
        TokGte -> return LogGte

data Expression
    = Expression Summation [(LogOperation, Summation)]
    deriving (Show)

expression :: Parser Expression
expression = do
    sm <- summation
    sms <- expression'
    return $ Expression sm sms

expression' :: Parser [(LogOperation, Summation)]
expression' = do
    res <- tryParse logOperation
    case res of
        Left _ -> return []
        Right op -> do
            sm <- summation
            pairs <- expression'
            return $ (op, sm):pairs

data Statement
    = StmtCompound [Statement]
    | StmtAssignment Identifier Expression
    | StmtCondition Expression Statement (Maybe Statement)
    | StmtForLoop Identifier Expression Expression Statement
    | StmtWhileLoop Expression Statement
    | StmtRead [Identifier]
    deriving (Show)

compound' :: Parser [Statement]
compound' = do
    stmt <- statement
    tt <- expectAny [TokBracketClose, TokSemicolon, TokNewline]
    case tt of
        TokBracketClose -> return [stmt]
        TokSemicolon -> do
            stmts <- compound'
            return (stmt:stmts)
        TokNewline -> do
            newlines
            tt <- lookahead
            case tt of
                TokBracketClose -> do
                    skip
                    return [stmt]
                _ -> do
                    stmts <- compound'
                    return (stmt:stmts)

compound :: Parser [Statement]
compound = do
    expect TokBracketOpen
    newlines
    compound'

assignment :: Parser (Identifier, Expression)
assignment = do
    ident <- identifier
    expect TokKwAs
    expr <- expression
    return (ident, expr)

condition :: Parser (Expression, Statement, Maybe Statement)
condition = do
    expect TokKwIf
    expr <- expression
    newlines
    expect TokKwThen
    thenBranch <- statement
    newlines
    tt <- lookahead
    maybeElseBranch <- if tt == TokKwElse
        then do
            skip
            elseBranch <- statement
            return $ Just elseBranch
        else
            return Nothing
    return (expr, thenBranch, maybeElseBranch)

forLoop :: Parser (Identifier, Expression, Expression, Statement)
forLoop = do
    expect TokKwFor
    (ident, initExpr) <- assignment
    expect TokKwTo
    targetExpr <- expression
    expect TokKwDo
    body <- statement
    return (ident, initExpr, targetExpr, body)

whileLoop :: Parser (Expression, Statement)
whileLoop = do
    expect TokKwWhile
    expr <- expression
    expect TokKwDo
    body <- statement
    return (expr, body)

readStmt' :: Parser [Identifier]
readStmt' = do
    tt <- expectAny [TokParenthesisClose, TokComma]
    case tt of
        TokParenthesisClose ->
            return []
        TokComma -> do
            ident <- identifier
            idents <- readStmt'
            return (ident:idents)

readStmt :: Parser [Identifier]
readStmt = do
    expect TokKwRead
    expect TokParenthesisOpen
    ident <- identifier
    idents <- readStmt'
    return (ident:idents)

statement :: Parser Statement
statement = do
    tt <- lookahead
    case tt of
        TokBracketOpen -> do
            stmts <- compound
            return $ StmtCompound stmts
        TokIdent _ -> do
            (ident, expr) <- assignment
            return $ StmtAssignment ident expr
        TokKwIf -> do
            (expr, thenBranch, maybeElseBranch) <- condition
            return $ StmtCondition expr thenBranch maybeElseBranch
        TokKwFor -> do
            (ident, initExpr, targetExpr, body) <- forLoop
            return $ StmtForLoop ident initExpr targetExpr body
        TokKwWhile -> do
            (expr, body) <- whileLoop
            return $ StmtWhileLoop expr body
        TokKwRead -> do
            idents <- readStmt
            return $ StmtRead idents
        _ -> parserError $ "Expected statement, found " ++ show tt

data Block
    = BlockDecl [Identifier] Type
    | BlockStmt Statement
    deriving (Show)

block :: Parser Block
block = do
    tt <- lookahead
    case tt of
        TokKwDim -> do
            (idents, type_) <- declaration
            return $ BlockDecl idents type_
        _ -> do
            stmt <- statement
            return $ BlockStmt stmt

blockSeparator :: Parser ()
blockSeparator = do
    _ <- expectAny [TokSemicolon, TokNewline]
    return ()

newtype Program = Program [Block]
    deriving (Show)

blocks :: Parser [Block]
blocks = do
    tt <- lookahead
    if tt == TokKwEnd
        then return []
        else do
            b <- block
            blockSeparator
            newlines
            bs <- blocks
            return $ b:bs

program :: Parser Program
program = do
    bs <- blocks
    expect TokKwEnd
    return $ Program bs

parse :: [Token] -> Either ParserError Program
parse = Parcomb.parse program
