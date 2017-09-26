module Pilya.Lex
    ( TokenType(..)
    , isTokIdent
    , identString
    , isTokInteger
    , integerValue
    , isTokReal
    , realValue
    , isTokNumber
    , Token(..)
    , ParserState(..)
    , Parser(..)
    , ParserError(..)
    , newParser
    , advance
    , parse
    , keywordsTable
    , operatorTable
    , EntryNumber (..)
    , entryNumber
    , ParserEntries (..)
    , Entry (..)
    , emptyParserEntries
    , pushEntry
    , toParserEntries
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad       (foldM)
import           Data.Char           (isDigit, isLetter)
import           Data.List           (elemIndex)
import           Data.Maybe          (fromJust, fromMaybe, isJust, isNothing)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Pilya.Table         as Tbl
import           Text.Read           (readMaybe)

data TokenType
    = TokNewline -- \n
    | TokSemicolon -- :
    | TokNeq -- <>
    | TokEq -- =
    | TokLt -- <
    | TokLte -- <=
    | TokGt -- >
    | TokGte -- >=
    | TokPlus -- +
    | TokMinus -- -
    | TokKwOr -- or
    | TokMult -- *
    | TokDiv -- /
    | TokKwAnd -- and
    | TokKwNot -- not
    | TokParenthesisOpen -- (
    | TokParenthesisClose -- )
    | TokKwTrue -- true
    | TokKwFalse -- false
    | TokIdent String
    | TokInteger Integer
    | TokReal Double
    | TokKwDim -- dim
    | TokPercent -- %, denotes integer type
    | TokExcl -- !, denotes real type
    | TokDollar -- $, denotes boolean type
    | TokBracketOpen -- [
    | TokBracketClose -- ]
    | TokKwAs -- as
    | TokKwIf -- if
    | TokKwThen -- then
    | TokKwElse -- else
    | TokKwFor -- for
    | TokKwTo -- to
    | TokKwDo -- do
    | TokKwWhile -- while
    | TokKwRead -- read
    | TokKwWrite -- write
    | TokComma -- ,
    | TokKwEnd -- end
    deriving (Show, Eq)

fromKeyword :: String -> Maybe TokenType
fromKeyword kw = case kw of
    "or"    -> Just TokKwOr
    "and"   -> Just TokKwAnd
    "not"   -> Just TokKwNot
    "true"  -> Just TokKwTrue
    "false" -> Just TokKwFalse
    "dim"   -> Just TokKwDim
    "as"    -> Just TokKwAs
    "if"    -> Just TokKwIf
    "then"  -> Just TokKwThen
    "else"  -> Just TokKwElse
    "for"   -> Just TokKwFor
    "to"    -> Just TokKwTo
    "do"    -> Just TokKwDo
    "while" -> Just TokKwWhile
    "read"  -> Just TokKwRead
    "write" -> Just TokKwWrite
    "end"   -> Just TokKwEnd
    _       -> Nothing

fromOperator :: String -> Maybe TokenType
fromOperator op = case op of
    ":"  -> Just TokSemicolon
    "<>" -> Just TokNeq
    "="  -> Just TokEq
    "<"  -> Just TokLt
    "<=" -> Just TokLte
    ">"  -> Just TokGt
    ">=" -> Just TokGte
    "+"  -> Just TokPlus
    "-"  -> Just TokMinus
    "*"  -> Just TokMult
    "/"  -> Just TokDiv
    "("  -> Just TokParenthesisOpen
    ")"  -> Just TokParenthesisClose
    "%"  -> Just TokPercent
    "!"  -> Just TokExcl
    "$"  -> Just TokDollar
    "["  -> Just TokBracketOpen
    "]"  -> Just TokBracketClose
    ","  -> Just TokComma
    _    -> Nothing

isTokIdent :: TokenType -> Bool
isTokIdent (TokIdent _) = True
isTokIdent _            = False

identString :: TokenType -> String
identString (TokIdent s) = s
identString tt           = error $ show tt ++ " is not an identifier"

isTokInteger :: TokenType -> Bool
isTokInteger (TokInteger _) = True
isTokInteger _              = False

integerValue :: TokenType -> Integer
integerValue (TokInteger i) = i
integerValue tt             = error $ show tt ++ " is not an integer"

isTokReal :: TokenType -> Bool
isTokReal (TokReal _) = True
isTokReal _           = False

realValue :: TokenType -> Double
realValue (TokReal d) = d
realValue tt          = error $ show tt ++ " is not a real number"

isTokNumber :: TokenType -> Bool
isTokNumber tt = isTokInteger tt || isTokReal tt

data Token = Token
    { tokenType   :: TokenType
    , tokenLine   :: Int
    , tokenPos    :: Int
    , tokenLength :: Int
    }
    deriving (Show)

data CharType
    = CharWhitespace -- usual space, \t, \r
    | CharNewline    -- \n
    | CharDigit      -- 0-9
    | CharLetter     -- a-z, A-Z
    | CharSpecial    -- special symbols: comma, period and so on
    | CharOther
    deriving (Eq)

charType :: Char -> CharType
charType ch
    | ch == ' ' = CharWhitespace
    | ch == '\t' = CharWhitespace
    | ch == '\r' = CharWhitespace
    | ch == '\n' = CharNewline
    | isDigit ch = CharDigit
    | isLetter ch = CharLetter
    | ch `elem` ":<>=+-*/()%!$[].,{}" = CharSpecial
    | otherwise = CharOther

data Sign = SignPos | SignNeg

data ParserState
    -- no token is being read
    = StateFree
    -- reading a comment, waiting for its end
    | StateComment
    -- reading an identier or a keyword;
    -- contains a buffer and token's start position
    | StateAlpha String Int
    -- reading a binary, octal, decimal or hex. number;
    -- contains a buffer and token's start position
    | StateInt String Int
    -- expecting sign of an exponent or continuation of a hex. number;
    -- contains a buffer and token's start position
    | StateHexOrExp String Int
    -- reading fractional part of a real number;
    -- contains whole part, buffer and token's start position
    | StateFrac Int String Int
    -- expecting a sign of an exponent;
    -- container mantissa and token's start position
    | StateExpSign Double Int
    -- reading value of an exponent;
    -- contains mantissa, exponent's sign and token's start position
    | StateExpVal Double Sign String Int
    -- reading an operator;
    -- contains a buffer and token's start position
    | StateOperator String Int

readBase :: Int -> String -> Maybe Int
readBase = readBase' 0

-- |Reads a number from string with given base.
readBase' :: Int -> Int -> String -> Maybe Int
readBase' acc _ "" = Just acc
readBase' acc base (c:cs)
    | isNothing charDigit = Nothing
    | fromJust charDigit >= base = Nothing
    | otherwise = readBase' (acc * base + fromJust charDigit) base cs
    where
        digit char
            | isDigit char = elemIndex char "0123456789"
            | char `elem` "abcdef" = (+ 10) <$> elemIndex char "abcdef"
            | char `elem` "ABCDEF" = (+ 10) <$> elemIndex char "ABCDEF"
            | otherwise = Nothing
        charDigit = digit c

-- |Parses an integer in language's syntax from __reversed__ string.
parseInt :: String -> Maybe Int
parseInt str
    | head str == 'b' || head str == 'B' =
        readBase 2 (reverse $ tail str)
    | head str == 'o' || head str == 'O' =
        readBase 8 (reverse $ tail str)
    | head str == 'd' || head str == 'D' =
        readBase 10 (reverse $ tail str)
    | head str == 'h' || head str == 'H' =
        readBase 16 (reverse $ tail str)
    | otherwise =
        readBase 10 (reverse str)

data Parser = Parser
    { parserState      :: ParserState
    , parserLine       :: Int
    , parserPos        :: Int
    -- during parsing token stack is reversed: next tokens are put on top of previous
    , parserTokenStack :: [Token]
    }

data ParserError = ParserError
    { errorLine :: Int
    , errorPos  :: Int
    , errorMsg  :: String
    }

newParser :: Parser
newParser = Parser StateFree 1 1 []

-- |Advances parsing process one char forward.
-- If the end of file has been reached, feed this function with `Nothing`
-- to finish parsing.
advance :: Parser -> Maybe Char -> Either ParserError Parser
advance (Parser state line pos tokens) char =
    case advResult of
        AdvError err ->
            Left err
        AdvNoToken newState ->
            Right $ Parser newState newLine newPos tokens
        AdvToken newState newTok ->
            Right $ Parser newState newLine newPos (newTok:tokens)
        AdvNotConsumed newState ->
            advance (Parser newState line pos tokens) char
        AdvNotConsumedToken newState newTok ->
            advance (Parser newState line pos (newTok:tokens)) char
    where
        advResult = advance' state line pos char
        ct = fmap charType char
        newLine = if ct == Just CharNewline then line+1 else line
        newPos = if ct == Just CharNewline then 1 else pos+1

data AdvanceResult
    = AdvError ParserError
    | AdvNoToken ParserState
    | AdvToken ParserState Token
    | AdvNotConsumed ParserState
    | AdvNotConsumedToken ParserState Token

advance' :: ParserState -> Int -> Int -> Maybe Char -> AdvanceResult
advance' StateFree line pos char
    | char == Just '{' =
        AdvNoToken StateComment
    | ct == Just CharWhitespace =
        AdvNoToken StateFree
    | ct == Just CharNewline =
        AdvToken StateFree (Token TokNewline line pos 1)
    | ct == Just CharLetter =
        AdvNoToken (StateAlpha [fromJust char] pos)
    | ct == Just CharDigit =
        AdvNoToken (StateInt [fromJust char] pos)
    | char == Just '.' =
        AdvNoToken (StateFrac 0 "" pos)
    | ct == Just CharSpecial =
        AdvNoToken (StateOperator [fromJust char] pos)
    | isNothing char =
        AdvNoToken StateFree
    where
        ct = fmap charType char
advance' StateComment line pos char
    | char == Just '}' =
        AdvNoToken StateFree
    | isJust char =
        AdvNoToken StateComment
    | isNothing char =
        AdvError $ ParserError line pos "Unexpected end of file during comment parsing"
advance' (StateAlpha buf tokpos) line _ char
    | ct == Just CharDigit || ct == Just CharLetter =
        AdvNoToken (StateAlpha newBuf tokpos)
    | otherwise =
        AdvNotConsumedToken StateFree (Token tokType line tokpos (length buf))
    where
        ct = fmap charType char
        newBuf = fromJust char : buf
        rbuf = reverse buf
        tokType = fromMaybe (TokIdent rbuf) $ fromKeyword rbuf
advance' (StateInt buf tokpos) line _ char
    | ct == Just CharDigit || ct == Just CharLetter =
        AdvNoToken (StateInt newBuf tokpos)
    | char == Just '.' =
        case readBase 10 $ reverse buf of
            Just num -> AdvNoToken (StateFrac num "" tokpos)
            Nothing  -> AdvError $ ParserError line tokpos "Real number has incorrect format"
    | otherwise =
        case parseInt buf of
            Just num -> AdvNotConsumedToken StateFree (token num)
            Nothing  -> AdvError $ ParserError line tokpos "Integer has incorrent format"
    where
        ct = fmap charType char
        newBuf = fromJust char : buf
        token num = Token (TokInteger $ fromIntegral num) line tokpos (length buf)
advance' (StateFrac whole buf tokpos) line pos char
    | ct == Just CharDigit =
        AdvNoToken (StateFrac whole newBuf tokpos)
    | char == Just 'e' || char == Just 'E' =
        case bufDouble of
            Just double ->
                AdvNoToken (StateExpSign double tokpos)
            Nothing ->
                AdvError $ ParserError line tokpos "Real number has incorrect format"
    | maybe False (`elem` [CharWhitespace, CharNewline, CharSpecial]) ct || isNothing char =
        case bufDouble of
            Just double ->
                AdvNotConsumedToken StateFree (Token (TokReal double) line tokpos (pos-tokpos))
            Nothing ->
                AdvError $ ParserError line tokpos "Real number has incorrect format"
    | otherwise =
        AdvError $ ParserError line tokpos "Real number has incorrect format"
    where
        ct = fmap charType char
        newBuf = fromJust char : buf
        bufDouble = do
            bufNum <- readMaybe $ reverse buf
            let bufDiv = 10 ^ length buf
            return $ fromIntegral whole + (bufNum / bufDiv)
advance' (StateExpSign mant tokpos) line _ char
    | char == Just '+' =
        AdvNoToken (StateExpVal mant SignPos "" tokpos)
    | char == Just '-' =
        AdvNoToken (StateExpVal mant SignNeg "" tokpos)
    | ct == Just CharDigit =
        AdvNotConsumed (StateExpVal mant SignPos "" tokpos)
    | otherwise =
        AdvError $ ParserError line tokpos "Real number has incorrect format"
    where
        ct = fmap charType char
advance' (StateExpVal mant sign buf tokpos) line pos char
    | ct == Just CharDigit =
        AdvNoToken (StateExpVal mant sign newBuf tokpos)
    | maybe False (`elem` [CharWhitespace, CharNewline, CharSpecial]) ct || isNothing char =
        case bufDouble of
            Just dbl ->
                AdvToken StateFree (Token (TokReal dbl) line tokpos (pos-tokpos))
            Nothing ->
                AdvError $ ParserError line tokpos "Real number has incorrect format"
    | otherwise =
        AdvError $ ParserError line tokpos "Real number has incorrect format"
    where
        ct = fmap charType char
        newBuf = fromJust char : buf
        buildDouble :: Double -> Sign -> Int -> Double
        buildDouble m s p =
            case s of
                SignPos -> m * 10 ^ p
                SignNeg -> m * 0.1 ^ p
        bufDouble = do
            pow <- readMaybe $ reverse buf
            let dbl = buildDouble mant sign pow
            return dbl
advance' (StateOperator buf tokpos) line _ char
    | ct == Just CharSpecial =
        AdvNoToken (StateOperator newBuf tokpos)
    | otherwise =
        case operResult of
            Just operTok -> AdvNotConsumedToken StateFree (Token operTok line tokpos (length buf))
            Nothing -> AdvError $ ParserError line tokpos ("Unknown operator " ++ reverse buf)
    where
        ct = fmap charType char
        newBuf = fromJust char : buf
        operResult = fromOperator $ reverse buf

-- |Performs tokenization of a text and returns token list or an error.
parse :: Text -> Either ParserError [Token]
parse text =
    case finResult of
        Left err     -> Left err
        Right parser -> Right $ reverse $ parserTokenStack parser
    where
        charQueue = fmap Just (T.unpack text) ++ [Nothing]
        finResult = foldM advance newParser charQueue

keywordsTable :: Tbl.Table TokenType
keywordsTable = Tbl.tableList
    [ TokKwOr -- or
    , TokKwAnd -- and
    , TokKwNot -- not
    , TokKwTrue -- true
    , TokKwFalse -- false
    , TokKwDim -- dim
    , TokKwAs -- as
    , TokKwIf -- if
    , TokKwThen -- then
    , TokKwElse -- else
    , TokKwFor -- for
    , TokKwTo -- to
    , TokKwDo -- do
    , TokKwWhile -- while
    , TokKwRead -- read
    , TokKwWrite -- write
    , TokKwEnd -- end
    ]

operatorTable :: Tbl.Table TokenType
operatorTable = Tbl.tableList
    [ TokNewline -- \n
    , TokSemicolon -- :
    , TokNeq -- <>
    , TokEq -- =
    , TokLt -- <
    , TokLte -- <=
    , TokGt -- >
    , TokGte -- >=
    , TokPlus -- +
    , TokMinus -- -
    , TokMult -- *
    , TokDiv -- /
    , TokParenthesisOpen -- (
    , TokParenthesisClose -- )
    , TokPercent -- %, denotes integer type
    , TokExcl -- !, denotes real type
    , TokDollar -- $, denotes boolean type
    , TokBracketOpen -- [
    , TokBracketClose -- ]
    , TokComma -- ,
    ]

data EntryNumber = NumInteger Integer | NumReal Double
    deriving (Eq)

instance Show EntryNumber where
    show (NumInteger int) = show int
    show (NumReal dbl)    = show dbl

entryNumber :: TokenType -> EntryNumber
entryNumber (TokInteger int) = NumInteger int
entryNumber (TokReal real)   = NumReal real
entryNumber tt               = error $ "Invalid token type " ++ show tt

data Entry = Entry
    { entryLine  :: Int
    , entryPos   :: Int
    , entryLen   :: Int
    , entryTable :: Int
    , entryIndex :: Int
    }
    deriving (Show)

data ParserEntries = ParserEntries
    { peIdentsTable  :: Tbl.Table String
    , peNumbersTable :: Tbl.Table EntryNumber
    , peEntries      :: [Entry]
    }
    deriving (Show)

emptyParserEntries :: ParserEntries
emptyParserEntries = ParserEntries Tbl.empty Tbl.empty []

pushEntry :: ParserEntries -> Token -> ParserEntries
pushEntry (ParserEntries identTable numTable entries) (Token tt line pos len)
    | isJust kwIndex =
        ParserEntries identTable numTable (entries ++ [entr 0 (fromJust kwIndex)])
    | isJust operIndex =
        ParserEntries identTable numTable (entries ++ [entr 1 (fromJust operIndex)])
    | isTokIdent tt && isJust identIndex =
        ParserEntries identTable numTable (entries ++ [entr 2 (fromJust identIndex)])
    | isTokIdent tt && isNothing identIndex =
        ParserEntries newIdentTable numTable (entries ++ [entr 2 newIdentIndex])
    | isTokNumber tt && isJust numIndex =
        ParserEntries identTable numTable (entries ++ [entr 3 (fromJust numIndex)])
    | isTokNumber tt && isNothing numIndex =
        ParserEntries identTable newNumTable (entries ++ [entr 3 newNumIndex])
    | otherwise =
        error $ "Unexpected TokenType " ++ show tt
    where
        kwIndex = Tbl.find keywordsTable tt
        operIndex = Tbl.find operatorTable tt
        identIndex = Tbl.find identTable $ identString tt
        (newIdentIndex, newIdentTable) = Tbl.append identTable $ identString tt
        numIndex = Tbl.find numTable $ entryNumber tt
        (newNumIndex, newNumTable) = Tbl.append numTable $ entryNumber tt
        entr tbl ind = Entry line pos len tbl ind

toParserEntries :: [Token] -> ParserEntries
toParserEntries = foldl pushEntry emptyParserEntries
