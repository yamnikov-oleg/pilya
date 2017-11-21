module Main where

import           Control.Monad      (forM_)
import qualified Data.Text.IO       as TIO
import qualified Pilya.Compile      as Comp
import qualified Pilya.Lex          as Lex
import qualified Pilya.Syn          as Syn
import qualified Pilya.Table        as Tbl
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

asmParseAndPrint :: String -> IO ()
asmParseAndPrint path = do
    text <- TIO.readFile path
    case Lex.parse text of
        Left (Lex.ParserError line pos msg) ->
            putStrLn ("L:" ++ show line ++ ":" ++ show pos ++ ": " ++ msg)
        Right tokens ->
            case Syn.parse tokens of
                Left (Syn.ParserError msg line pos) ->
                    putStrLn ("S:" ++ show line ++ ":" ++ show pos ++ ": " ++ show msg)
                Right prg ->
                    case Comp.compile prg of
                        Left (Comp.Error cur msg) ->
                            putStrLn $ "C:" ++ show (Syn.cursorLine cur) ++ ":" ++ show (Syn.cursorPos cur) ++ ": " ++ msg
                        Right instr ->
                            forM_ (Tbl.toEnumList instr) (\(i, ins) -> putStrLn $ show i ++ " @ " ++ show ins)
printUsage :: IO ()
printUsage = do
    putStrLn "Usage:"
    putStrLn "    pilya-cli <command> [options]"
    putStrLn ""
    putStrLn "Supported commands:"
    putStrLn "    lex <filepath>  - perform lexical analysis (tokenization)"
    putStrLn "    syn <filepath>  - perform syntax tree parsing"
    putStrLn "    comp <filepath> - compile into VM instructions"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["lex", path] -> lexParseAndPrint path
        ["syn", path] -> synParseAndPrint path
        ["asm", path] -> asmParseAndPrint path
        _             -> printUsage
