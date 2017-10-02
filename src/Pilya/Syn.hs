module Pilya.Syn
    ( parse
    )
    where

import           Pilya.Lex     (Token (..), TokenType (..))
import           Pilya.Parcomb (Parser (..), ParserError (..), consume)
import qualified Pilya.Parcomb as Parcomb

testParse :: Parser (TokenType, TokenType, TokenType)
testParse = do
    t1 <- consume
    t2 <- consume
    t3 <- consume
    return (t1, t2, t3)

parse :: [Token] -> Either ParserError (TokenType, TokenType, TokenType)
parse = Parcomb.parse testParse
