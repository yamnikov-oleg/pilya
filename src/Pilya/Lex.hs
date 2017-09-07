module Pilya.Lex
    ( TokenType(..)
    , Token(..)
    , ParserState(..)
    , Parser(..)
    , ParserError(..)
    , newParser
    , advance
    , parse
    ) where

import           Control.Monad (foldM)
import           Data.Char     (isDigit, isLetter)
import           Data.Maybe    (fromJust, fromMaybe, isNothing)
import           Data.Text     (Text)
import qualified Data.Text     as T

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
    deriving (Show)

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

data Token = Token
    { tokenType :: TokenType
    , tokenLine :: Int
    , tokenPos  :: Int
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
    | ch `elem` ":<>=+-*/()%!$[],{}" = CharSpecial
    | otherwise = CharOther

data ParserState
    -- no token is being read
    = StateFree
    -- reading a comment, waiting for its end
    | StateComment
    -- reading an identier or a keyword;
    -- contains a buffer and token's start position
    | StateAlpha String Int
    -- reading a number;
    -- contains a buffer and token's start position
    | StateNumber String Int
    -- reading an operator;
    -- contains a buffer and token's start position
    | StateOperator String Int

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
advance (Parser StateFree line pos tokens) char
    | ct == Just CharWhitespace =
        Right $ Parser StateFree line (pos+1) tokens
    | ct == Just CharNewline =
        Right $ Parser StateFree (line+1) 1 (Token TokNewline line pos : tokens)
    | ct == Just CharLetter =
        Right $ Parser (StateAlpha [fromJust char] pos) line (pos+1) tokens
    | isNothing char =
        Right $ Parser StateFree line pos tokens
    where
        ct = fmap charType char
advance (Parser (StateAlpha buf tokpos) line pos tokens) char
    | ct == Just CharDigit || ct == Just CharLetter =
        Right $ Parser (StateAlpha newBuf tokpos) line (pos+1) tokens
    | otherwise =
        advance (Parser StateFree line pos (Token tokType line tokpos : tokens)) char
    where
        ct = fmap charType char
        newBuf = buf ++ [fromJust char]
        tokType = fromMaybe (TokIdent buf) $ fromKeyword buf

-- |Performs tokenization of a text and returns token list or an error.
parse :: Text -> Either ParserError [Token]
parse text =
    case finResult of
        Left err     -> Left err
        Right parser -> Right $ reverse $ parserTokenStack parser
    where
        charQueue = fmap Just (T.unpack text) ++ [Nothing]
        finResult = foldM advance newParser charQueue
