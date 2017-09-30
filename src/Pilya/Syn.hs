module Pilya.Syn
    ( ErrorMsg
    , parse
    )
    where

import           Pilya.Lex (Token (..), TokenType (..))

{-

parseIf :: Parser IfExpr
parseIf = do
    token TokIf
    iterVar <- ident
    token TokEq
    fromVal <- parseExpr
    token TokTo
    toVal <- parseExpr
    return $ IfExpr iterVar fromVal toVal

token :: TokenType -> Parser ()
token tt = do
    token <- lookahead
    if tokenType token == tt
        then consume
        else parseError "Expected token " ++ show tt

-}

newtype ErrorMsg = ErrorMsg String
    deriving (Show)

newtype Parser a = Parser
    { runParser :: [Token] -> (Either ErrorMsg a, [Token])
    }

instance Functor Parser where
    fmap f parser = Parser (\tokens ->
        let
            (result, rest) = runParser parser tokens
        in
            (fmap f result, rest))

instance Applicative Parser where
    pure x = Parser (\tokens -> (Right x, tokens))
    pf <*> px = Parser (\tokens ->
        let
            (resf, restf) = runParser pf tokens
            (resx, restx) = runParser px restf
        in
            (resf <*> resx, restx))

parserError :: String -> Parser a
parserError msg = Parser runParserError
    where
        runParserError tokens = (Left $ ErrorMsg msg, tokens)

instance Monad Parser where
    pa >>= pf = Parser (\tokens ->
        let
            (resa, resta) = runParser pa tokens
        in
            case resa of
                Left err  -> (Left err, resta)
                Right val -> runParser (pf val) resta)
    fail = parserError

lookahead :: Parser TokenType
lookahead = Parser runLookahead
    where
        runLookahead []     = (Left $ ErrorMsg "Unexpected EOF", [])
        runLookahead (t:ts) = (Right $ tokenType t, t:ts)

consume :: Parser TokenType
consume = Parser runConsume
    where
        runConsume []     = (Left $ ErrorMsg "Unexpected EOF", [])
        runConsume (t:ts) = (Right $ tokenType t, ts)

testParse :: Parser (TokenType, TokenType, TokenType)
testParse = do
    t1 <- consume
    t2 <- consume
    t3 <- consume
    return (t1, t2, t3)

parse :: [Token] -> Either ErrorMsg (TokenType, TokenType, TokenType)
parse tokens = fst $ runParser testParse tokens
