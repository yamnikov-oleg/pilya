module Pilya.Syn
    ( parse
    , ParserError (..)
    )
    where

import           Pilya.Lex     (Token (..), TokenType (..))
import           Pilya.Parcomb (Parser (..), ParserError (..), consume, expect,
                                expectAny, lookahead, many1sep, parserError,
                                skip, tryParse)
import qualified Pilya.Parcomb as Parcomb

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

data Statement
    = StmtCompound [Statement]
    | StmtAssignment Identifier Summation
    deriving (Show)

assignment :: Parser (Identifier, Summation)
assignment = do
    ident <- identifier
    expect TokKwAs
    sm <- summation
    return (ident, sm)

statement :: Parser Statement
statement = do
    tt <- lookahead
    case tt of
        TokIdent _ -> do
            (ident, sm) <- assignment
            return $ StmtAssignment ident sm
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
            bs <- blocks
            return $ b:bs

program :: Parser Program
program = do
    bs <- blocks
    expect TokKwEnd
    return $ Program bs

parse :: [Token] -> Either ParserError Program
parse = Parcomb.parse program
