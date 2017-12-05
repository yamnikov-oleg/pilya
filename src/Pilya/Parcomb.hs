module Pilya.Parcomb
    ( ErrorMsg (..)
    , Parser (..)
    , parserError
    , lookahead'
    , lookahead
    , lookbehind'
    , lookbehind
    , consume
    , skip
    , expect
    , expectAny
    , tryParse
    , many0
    , many1
    , many0sep
    , many1sep
    , suffixed
    , trace
    , traceStack
    , Cursor (..)
    , cursor
    , cursorAfter
    , cursorBehind
    , ParserError (..)
    , parse
    ) where

import           Data.List   (intercalate)
import qualified Debug.Trace as Dbg
import           Pilya.Lex   (Token (..), TokenType (..))

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

newtype ErrorMsg = ErrorMsg String

instance Show ErrorMsg where
    show (ErrorMsg s) = s

data ParserResult a = ParserResult
    { resultValue :: Either ErrorMsg a
    , resultLast  :: Maybe Token
    , resultRest  :: [Token]
    }

instance Functor ParserResult where
    fmap f (ParserResult val lst rest) = ParserResult (fmap f val) lst rest

newtype Parser a = Parser
    { runParser :: Maybe Token -> [Token] -> ParserResult a
    }

instance Functor Parser where
    fmap f parser = Parser (\lasttok tokens ->
        let
            result = runParser parser lasttok tokens
        in
            fmap f result)

instance Applicative Parser where
    pure x = Parser (ParserResult (Right x))
    pf <*> px = Parser (\lastTok tokens ->
        let
            ParserResult valf lastf restf = runParser pf lastTok tokens
            ParserResult valx lastx restx = runParser px lastf restf
        in
            ParserResult (valf <*> valx) lastx restx)

parserError :: String -> Parser a
parserError msg = Parser runParserError
    where
        runParserError = ParserResult (Left $ ErrorMsg msg)

instance Monad Parser where
    pa >>= pf = Parser (\lastTok tokens ->
        let
            ParserResult resa lasta resta = runParser pa lastTok tokens
        in
            case resa of
                Left err  -> ParserResult (Left err) lasta resta
                Right val -> runParser (pf val) lasta resta)
    fail = parserError

lookahead' :: Parser Token
lookahead' = Parser runLookahead
    where
        runLookahead lastTok []     = ParserResult (Left $ ErrorMsg "Unexpected EOF") lastTok []
        runLookahead lastTok (t:ts) = ParserResult (Right t) lastTok (t:ts)

lookahead :: Parser TokenType
lookahead = do
    tok <- lookahead'
    return $ tokenType tok

lookbehind' :: Parser (Maybe Token)
lookbehind' = Parser runLookbehind
    where
        runLookbehind lastTok = ParserResult (Right lastTok) lastTok

lookbehind :: Parser (Maybe TokenType)
lookbehind = do
    t <- lookbehind'
    return $ fmap tokenType t

consume :: Parser TokenType
consume = Parser runConsume
    where
        runConsume lastTok []     = ParserResult (Left $ ErrorMsg "Unexpected EOF") lastTok []
        runConsume _ (t:ts) = ParserResult (Right $ tokenType t) (Just t) ts

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
tryParse parser = Parser (\lastTok tokens ->
    case runParser parser lastTok tokens of
        ParserResult (Left msg) _ _         -> ParserResult (Right $ Left msg) lastTok tokens
        ParserResult (Right x) newLast rest -> ParserResult (Right $ Right x) newLast rest)

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

many1sep :: Parser () -> Parser a -> Parser [a]
many1sep sep p = do
    x <- p
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

trace :: String -> Parser a -> Parser a
trace msg p = Parser (\lastTok tokens ->
    Dbg.trace (msg ++ show (take 1 tokens)) (runParser p lastTok tokens))

traceStack :: String -> Parser a -> Parser a
traceStack msg p = Parser (\lastTok tokens ->
    Dbg.traceStack (msg ++ show (take 1 tokens)) (runParser p lastTok tokens))

data Cursor = Cursor
    { cursorLine :: Int
    , cursorPos  :: Int
    }

instance Show Cursor where
    show (Cursor line pos) = show (line, pos)

cursor :: Parser Cursor
cursor = do
    tok <- lookahead'
    return $ Cursor (tokenLine tok) (tokenPos tok)

cursorAfter :: Parser Cursor
cursorAfter = do
    tok <- lookahead'
    return $ Cursor (tokenLine tok) (tokenPos tok + tokenLength tok)

cursorBehind :: Parser Cursor
cursorBehind = do
    mt <- lookbehind'
    case mt of
        Just t  -> return $ Cursor (tokenLine t) (tokenPos t + tokenLength t)
        Nothing -> return $ Cursor 1 1

data ParserError = ParserError
    { errorMsg  :: ErrorMsg
    , errorLine :: Int
    , errorPos  :: Int
    }

parse :: Parser a -> [Token] -> Either ParserError a
parse parser tokens = case parseResult of
    ParserResult (Left msg) lastTok []    -> Left $ ParserError msg (lastLine lastTok) (lastPos lastTok)
    ParserResult (Left msg) _ (t:_) -> Left $ ParserError msg (tokenLine t) (tokenPos t)
    ParserResult (Right val) _ _          -> Right val
    where
        parseResult = runParser parser Nothing tokens
        lastLine Nothing  = 1
        lastLine (Just t) = tokenLine t
        lastPos Nothing  = 1
        lastPos (Just t) = tokenPos t
