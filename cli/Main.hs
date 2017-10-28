module Main where

import           Control.Monad      (forM_)
import qualified Data.Text.IO       as TIO
import qualified Pilya.Lex          as Lex
import qualified Pilya.Syn          as Syn
import           Pilya.Traverse     (ASTTraversible (..))
import           System.Environment (getArgs)

lexParseAndPrint :: String -> IO ()
lexParseAndPrint path = do
    text <- TIO.readFile path
    case Lex.parse text of
        (Left (Lex.ParserError line pos msg)) ->
            putStrLn (show line ++ ":" ++ show pos ++ ": " ++ msg)
        (Right tokens) ->
            mapM_ print tokens

synPrint :: Syn.Node String -> Int -> IO Int
synPrint (Syn.Node text start end) indent = do
    let indentStr = concat (replicate indent "|   ")
    putStrLn $ indentStr ++ text ++ "   " ++ show start ++ " - " ++ show end
    return $ indent + 1

synParseAndPrint :: String -> IO ()
synParseAndPrint path = do
    text <- TIO.readFile path
    case Lex.parse text of
        Left (Lex.ParserError line pos msg) ->
            putStrLn (show line ++ ":" ++ show pos ++ ": " ++ msg)
        Right tokens ->
            case Syn.parse tokens of
                Left (Syn.ParserError msg line pos) ->
                    putStrLn (show line ++ ":" ++ show pos ++ ": " ++ show msg)
                Right prg ->
                    asttraverse synPrint prg 0

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
        ["syn", path] -> synParseAndPrint path
        _             -> printUsage
