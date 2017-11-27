module Main where

import           Control.Monad      (forM_)
import           Data.Text          (Text)
import qualified Data.Text.IO       as TIO
import qualified Pilya.Compile      as Comp
import qualified Pilya.Exec         as Exec
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
                            forM_ (Tbl.toEnumList instr) (\(i, (line, ins)) -> putStrLn $ show i ++ " @ " ++ show line ++ " : " ++ show ins)

showLexError :: Lex.ParserError -> String
showLexError (Lex.ParserError line pos msg) =
    "L:" ++ show line ++ ":" ++ show pos ++ ": " ++ msg

showSynError :: Syn.ParserError -> String
showSynError (Syn.ParserError msg line pos) =
    "S:" ++ show line ++ ":" ++ show pos ++ ": " ++ show msg

showCompError :: Comp.Error -> String
showCompError (Comp.Error cur msg) =
    "C:" ++ show (Syn.cursorLine cur) ++ ":" ++ show (Syn.cursorPos cur) ++ ": " ++ msg

mapErr :: (ea -> eb) -> Either ea a -> Either eb a
mapErr _ (Right x) = Right x
mapErr f (Left e)  = Left (f e)

compile :: Text -> Either String (Tbl.Table Exec.Instruction)
compile text = do
    tokens <- mapErr showLexError $ Lex.parse text
    ast <- mapErr showSynError $ Syn.parse tokens
    itbl <- mapErr showCompError $ Comp.compile ast
    return $ fmap snd itbl

getInput :: [String] -> IO [String]
getInput inp = do
    line <- getLine
    if line == ""
        then return inp
        else getInput (inp ++ [line])

parseAndRun :: String -> IO ()
parseAndRun path = do
    text <- TIO.readFile path
    case compile text of
        Left err -> putStrLn err
        Right instbl -> do
            putStrLn "Enter program's input. Put empty line when finished."
            inp <- getInput []
            case Exec.run instbl inp of
                Left (ind, err) -> putStrLn $ show ind ++ " : " ++ err
                Right outp      -> forM_ (reverse outp) putStrLn

printUsage :: IO ()
printUsage = do
    putStrLn "Usage:"
    putStrLn "    pilya-cli <command> [options]"
    putStrLn ""
    putStrLn "Supported commands:"
    putStrLn "    lex <filepath>  - perform lexical analysis (tokenization)"
    putStrLn "    syn <filepath>  - perform syntax tree parsing"
    putStrLn "    asm <filepath>  - compile into VM instructions"
    putStrLn "    run <filepath>  - compile and run"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["lex", path] -> lexParseAndPrint path
        ["syn", path] -> synParseAndPrint path
        ["asm", path] -> asmParseAndPrint path
        ["run", path] -> parseAndRun path
        _             -> printUsage
