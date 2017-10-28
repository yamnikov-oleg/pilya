module Pilya.Syn
    ( Identifier
    , NIdentifier
    , Type (..)
    , NType
    , NumberLiteral (..)
    , NNumberLiteral
    , BoolLiteral (..)
    , NBoolLiteral
    , Multiplier (..)
    , NMultiplier
    , MultOperation (..)
    , NMultOperation
    , Multiplication (..)
    , NMultiplication
    , SumOperation (..)
    , NSumOperation
    , Summation (..)
    , NSummation
    , LogOperation (..)
    , NLogOperation
    , Expression (..)
    , NExpression
    , Statement (..)
    , NStatement
    , Block (..)
    , NBlock
    , Program (..)
    , NProgram
    , parse
    , ParserError (..)
    , ErrorMsg (..)
    , Node (..)
    , Cursor (..)
    )
    where

import           Control.Monad (when)
import           Pilya.Lex     (Token (..), TokenType (..))
import           Pilya.Parcomb (Cursor, ErrorMsg (..), Parser (..),
                                ParserError (..), consume, cursor, cursorBehind,
                                expect, expectAny, lookahead, many1sep,
                                parserError, skip, tryParse)
import qualified Pilya.Parcomb as Parcomb

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

newlines :: Parser ()
newlines = do
    tt <- lookahead
    when (tt == TokNewline) $ do
        skip
        newlines

data Node a = Node
    { nodeValue :: a
    , nodeStart :: Cursor
    , nodeEnd   :: Cursor
    }

instance (Show a) => Show (Node a) where
    show (Node val _ _) = show val

instance Functor Node where
    fmap f (Node val start end) = Node (f val) start end

node :: Parser a -> Parser (Node a)
node p = do
    start <- cursor
    val <- p
    end <- cursorBehind
    return $ Node val start end

type Identifier = String

type NIdentifier = Node Identifier

data Type
    = TypeInt
    | TypeReal
    | TypeBool
    deriving (Show)

type NType = Node Type

typeFromToken :: TokenType -> Type
typeFromToken TokPercent = TypeInt
typeFromToken TokExcl = TypeReal
typeFromToken TokDollar = TypeBool
typeFromToken tt = error $ "Token " ++ show tt ++ " does not denote a type"

identifier :: Parser NIdentifier
identifier = node $ do
    tt <- lookahead
    case tt of
        TokIdent s -> do { consume; return s }
        _          -> parserError "Expected identifier"

declaration :: Parser ([NIdentifier], NType)
declaration = do
    expect TokKwDim
    idents <- many1sep (expect TokComma) identifier
    typeTok <- node $ expectAny [TokPercent, TokExcl, TokDollar]
    return (idents, fmap typeFromToken typeTok)

data NumberLiteral
    = NumInteger Integer
    | NumReal Double
    deriving (Show)

type NNumberLiteral = Node NumberLiteral

numberLiteral :: Parser NNumberLiteral
numberLiteral = node $ do
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

type NBoolLiteral = Node BoolLiteral

boolLiteral :: Parser NBoolLiteral
boolLiteral = node $ do
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
    = MultIdent NIdentifier
    | MultNumber NNumberLiteral
    | MultBool NBoolLiteral
    | MultNot NMultiplier
    | MultGrouped NExpression
    deriving (Show)

type NMultiplier = Node Multiplier

multiplier :: Parser NMultiplier
multiplier = node $ do
    tt <- lookahead
    case tt of
        TokIdent _ ->
            fmap MultIdent identifier
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
        TokParenthesisOpen -> do
            skip
            expr <- expression
            expect TokParenthesisClose
            return $ MultGrouped expr
        _ -> parserError $ "Expected multiplier, got " ++ show tt

data MultOperation
    = MultOpMult -- *
    | MultOpDiv  -- /
    | MultOpAnd  -- and
    deriving (Show)

type NMultOperation = Node MultOperation

multOperation :: Parser NMultOperation
multOperation = node $ do
    tt <- expectAny [TokMult, TokDiv, TokKwAnd]
    case tt of
        TokMult  -> return MultOpMult
        TokDiv   -> return MultOpDiv
        TokKwAnd -> return MultOpAnd

data Multiplication
    = Multiplication NMultiplier [(NMultOperation, NMultiplier)]
    deriving (Show)

type NMultiplication = Node Multiplication

multiplication :: Parser NMultiplication
multiplication = node $ do
    m <- multiplier
    ms <- multiplication'
    return $ Multiplication m ms

multiplication' :: Parser [(NMultOperation, NMultiplier)]
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

type NSumOperation = Node SumOperation

sumOperation :: Parser NSumOperation
sumOperation = node $ do
    tt <- expectAny [TokPlus, TokMinus, TokKwOr]
    case tt of
        TokPlus  -> return SumPlus
        TokMinus -> return SumMinus
        TokKwOr  -> return SumOr

data Summation
    = Summation NMultiplication [(NSumOperation, NMultiplication)]
    deriving (Show)

type NSummation = Node Summation

summation :: Parser NSummation
summation = node $ do
    m <- multiplication
    ms <- summation'
    return $ Summation m ms

summation' :: Parser [(NSumOperation, NMultiplication)]
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

type NLogOperation = Node LogOperation

logOperation :: Parser NLogOperation
logOperation = node $ do
    tt <- expectAny [TokNeq, TokEq, TokLt, TokLte, TokGt, TokGte]
    case tt of
        TokNeq -> return LogNeq
        TokEq  -> return LogEq
        TokLt  -> return LogLt
        TokLte -> return LogLte
        TokGt  -> return LogGt
        TokGte -> return LogGte

data Expression
    = Expression NSummation [(NLogOperation, NSummation)]
    deriving (Show)

type NExpression = Node Expression

expression :: Parser NExpression
expression = node $ do
    sm <- summation
    sms <- expression'
    return $ Expression sm sms

expression' :: Parser [(NLogOperation, NSummation)]
expression' = do
    res <- tryParse logOperation
    case res of
        Left _ -> return []
        Right op -> do
            sm <- summation
            pairs <- expression'
            return $ (op, sm):pairs

data Statement
    = StmtCompound [NStatement]
    | StmtAssignment NIdentifier NExpression
    | StmtCondition NExpression NStatement (Maybe NStatement)
    | StmtForLoop NIdentifier NExpression NExpression NStatement
    | StmtWhileLoop NExpression NStatement
    | StmtRead [NIdentifier]
    | StmtWrite [NExpression]
    deriving (Show)

type NStatement = Node Statement

compound' :: Parser [NStatement]
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

compound :: Parser [NStatement]
compound = do
    expect TokBracketOpen
    newlines
    compound'

assignment :: Parser (NIdentifier, NExpression)
assignment = do
    ident <- identifier
    expect TokKwAs
    expr <- expression
    return (ident, expr)

condition :: Parser (NExpression, NStatement, Maybe NStatement)
condition = do
    expect TokKwIf
    expr <- expression
    newlines
    expect TokKwThen
    thenBranch <- statement
    res <- tryParse $ do
        newlines
        expect TokKwElse
    maybeElseBranch <- case res of
        Right _ -> do
            elseBranch <- statement
            return $ Just elseBranch
        Left _ ->
            return Nothing
    return (expr, thenBranch, maybeElseBranch)

forLoop :: Parser (NIdentifier, NExpression, NExpression, NStatement)
forLoop = do
    expect TokKwFor
    (ident, initExpr) <- assignment
    expect TokKwTo
    targetExpr <- expression
    expect TokKwDo
    body <- statement
    return (ident, initExpr, targetExpr, body)

whileLoop :: Parser (NExpression, NStatement)
whileLoop = do
    expect TokKwWhile
    expr <- expression
    expect TokKwDo
    body <- statement
    return (expr, body)

readStmt' :: Parser [NIdentifier]
readStmt' = do
    tt <- expectAny [TokParenthesisClose, TokComma]
    case tt of
        TokParenthesisClose ->
            return []
        TokComma -> do
            ident <- identifier
            idents <- readStmt'
            return (ident:idents)

readStmt :: Parser [NIdentifier]
readStmt = do
    expect TokKwRead
    expect TokParenthesisOpen
    ident <- identifier
    idents <- readStmt'
    return (ident:idents)

writeStmt' :: Parser [NExpression]
writeStmt' = do
    tt <- expectAny [TokParenthesisClose, TokComma]
    case tt of
        TokParenthesisClose ->
            return []
        TokComma -> do
            expr <- expression
            exprs <- writeStmt'
            return (expr:exprs)

writeStmt :: Parser [NExpression]
writeStmt = do
    expect TokKwWrite
    expect TokParenthesisOpen
    expr <- expression
    exprs <- writeStmt'
    return (expr:exprs)

statement :: Parser NStatement
statement = node $ do
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
        TokKwWrite -> do
            exprs <- writeStmt
            return $ StmtWrite exprs
        _ -> parserError $ "Expected statement, found " ++ show tt

data Block
    = BlockDecl [NIdentifier] NType
    | BlockStmt NStatement
    deriving (Show)

type NBlock = Node Block

block :: Parser NBlock
block = node $ do
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

newtype Program = Program [NBlock]
    deriving (Show)

type NProgram = Node Program

blocks :: Parser [NBlock]
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

program :: Parser NProgram
program = node $ do
    bs <- blocks
    expect TokKwEnd
    return $ Program bs

parse :: [Token] -> Either ParserError NProgram
parse = Parcomb.parse program
