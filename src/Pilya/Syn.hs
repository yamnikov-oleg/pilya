module Pilya.Syn
    ( parse
    , ParserError (..)
    )
    where

import           Pilya.Lex     (Token (..), TokenType (..))
import           Pilya.Parcomb (Parser (..), ParserError (..), consume, expect,
                                expectAny, lookahead, many1sep, parserError)
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

data Statement
    = StmtCompound [Statement]
    | StmtAssignment Identifier
    deriving (Show)

assignment :: Parser (Identifier, ())
assignment = do
    ident <- identifier
    expect TokKwAs
    return (ident, ())

statement :: Parser Statement
statement = do
    tt <- lookahead
    case tt of
        TokIdent _ -> do
            (ident, _) <- assignment
            return $ StmtAssignment ident
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
