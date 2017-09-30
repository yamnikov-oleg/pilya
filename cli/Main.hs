module Main where

import qualified Data.Text.IO       as TIO
import           Pilya.Lex
import           System.Environment (getArgs)

lexParseAndPrint :: String -> IO ()
lexParseAndPrint path = do
    text <- TIO.readFile path
    case parse text of
        (Left (ParserError line pos msg)) ->
            putStrLn (show line ++ ":" ++ show pos ++ ": " ++ msg)
        (Right tokens) ->
            mapM_ print tokens

printUsage :: IO ()
printUsage = do
    putStrLn "Usage:"
    putStrLn "    pilya-cli <command> [options]"
    putStrLn ""
    putStrLn "Supported commands:"
    putStrLn "    lex <filepath> - perform lexical analysis (tokenization)"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["lex", path] -> lexParseAndPrint path
        _             -> printUsage
