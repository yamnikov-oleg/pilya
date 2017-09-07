module Main where

import qualified Data.Text.IO       as TIO
import           Pilya.Lex
import           System.Environment (getArgs)

printParseResult :: Either ParserError [Token] -> IO ()
printParseResult (Left (ParserError line pos msg)) =
    putStrLn (show line ++ ":" ++ show pos ++ ": " ++ msg)
printParseResult (Right tokens) =
    mapM_ print tokens

parseAndPrint :: String -> IO ()
parseAndPrint path = do
    text <- TIO.readFile path
    printParseResult $ parse text

main :: IO ()
main = do
    args <- getArgs
    mapM_ parseAndPrint args
