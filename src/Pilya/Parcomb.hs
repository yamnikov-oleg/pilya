module Pilya.Parcomb
    ( ErrorMsg
    , Parser (..)
    , parserError
    , lookahead
    , consume
    , skip
    , expect
    , expectAny
    , tryParse
    , many0
    , many1
    , many0sep
    , suffixed
    , ParserError (..)
    , parse
    ) where

import           Data.List (intercalate)
import           Pilya.Lex (Token (..), TokenType (..))

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

skip :: Parser ()
skip = do
    _ <- consume
    return ()

expect :: TokenType -> Parser ()
expect et = do
    at <- lookahead
    if at == et
        then skip
        else parserError $ "Expected token " ++ show et

expectAny :: [TokenType] -> Parser TokenType
expectAny tts = do
    at <- lookahead
    if at `elem` tts
        then consume
        else parserError $ "Expected one of tokens: " ++ intercalate ", " (map show tts)

tryParse :: Parser a -> Parser (Either ErrorMsg a)
tryParse parser = Parser (\tokens ->
    case runParser parser tokens of
        (Left msg, _)   -> (Right $ Left msg, tokens)
        (Right x, rest) -> (Right $ Right x, rest))

many0 :: Parser a -> Parser [a]
many0 p = do
    res <- tryParse p
    case res of
        Left _ -> return []
        Right x -> do
            xs <- many0 p
            return $ x:xs

many1 :: Parser a -> Parser [a]
many1 p = do
    x <- p
    xs <- many0 p
    return $ x:xs

many0sep :: Parser () -> Parser a -> Parser [a]
many0sep sep p = do
    res <- tryParse p
    case res of
        Left _ -> return []
        Right x -> do
            xs <- many0sep' sep p
            return $ x:xs

many0sep' :: Parser () -> Parser a -> Parser [a]
many0sep' sep p = do
    res <- tryParse sep
    case res of
        Left _ -> return []
        Right _ -> do
            x <- p
            xs <- many0sep' sep p
            return $ x:xs

suffixed :: Parser a -> Parser b -> Parser a
suffixed pa pb = do
    xa <- pa
    _ <- pb
    return xa

data ParserError = ParserError
    { errorMsg  :: ErrorMsg
    , errorLine :: Int
    , errorPos  :: Int
    }

parse :: Parser a -> [Token] -> Either ParserError a
parse parser tokens = case parseResult of
    (Left msg, [])  -> Left $ ParserError msg lastLine lastPos
    (Left msg, t:_) -> Left $ ParserError msg (tokenLine t) (tokenPos t)
    (Right val, _)  -> Right val
    where
        parseResult = runParser parser tokens
        (lastLine, lastPos) = case tokens of
            [] -> (1, 1)
            t -> let lt = last t in (tokenLine lt, tokenPos lt + tokenLength lt)
