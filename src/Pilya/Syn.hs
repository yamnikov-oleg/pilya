module Pilya.Syn
    ( parse
    , ParserError (..)
    )
    where

import           Pilya.Lex     (Token (..), TokenType (..))
import           Pilya.Parcomb (Parser (..), ParserError (..), consume, expect,
                                expectAny, lookahead, parserError)
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

data Declaration = Declaration [Identifier] Type
    deriving (Show)

identifier :: Parser Identifier
identifier = do
    tt <- lookahead
    case tt of
        TokIdent s -> do { consume; return s }
        _          -> parserError "Expected identifier"

declaration :: Parser Declaration
declaration = do
    expect TokKwDim
    ident <- identifier
    typeTok <- expectAny [TokPercent, TokExcl, TokDollar]
    return $ Declaration [ident] (typeFromToken typeTok)

data Block
    = BlockDecl Declaration
    | BlockStmt
    deriving (Show)

block :: Parser Block
block = do
    decl <- declaration
    return $ BlockDecl decl

newtype Program = Program [Block]
    deriving (Show)

program :: Parser Program
program = do
    b <- block
    expect TokKwEnd
    return $ Program [b]

parse :: [Token] -> Either ParserError Program
parse = Parcomb.parse program
