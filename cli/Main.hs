module Main where

import           Control.Monad      (forM_)
import qualified Data.Text.IO       as TIO
import qualified Pilya.Lex          as Lex
import qualified Pilya.Syn          as Syn
import           System.Environment (getArgs)

lexParseAndPrint :: String -> IO ()
lexParseAndPrint path = do
    text <- TIO.readFile path
    case Lex.parse text of
        (Left (Lex.ParserError line pos msg)) ->
            putStrLn (show line ++ ":" ++ show pos ++ ": " ++ msg)
        (Right tokens) ->
            mapM_ print tokens

printIndented :: Int -> String -> IO ()
printIndented indent val = putStrLn $ concat (replicate indent "|   ") ++ val

synPrintMultOpPair :: Int -> (Syn.MultOperation, Syn.Multiplier) -> IO ()
synPrintMultOpPair indent (op, mulr) = do
    printIndented indent (show op)
    synPrintMultiplier indent mulr

synPrintMultiplier :: Int -> Syn.Multiplier -> IO ()
synPrintMultiplier indent (Syn.MultIdent ident) =
    printIndented indent $ "MultIndent " ++ ident
synPrintMultiplier indent (Syn.MultNumber num) =
    printIndented indent $ "MultNumber " ++ show num
synPrintMultiplier indent (Syn.MultBool bool) =
    printIndented indent $ "MultNumber " ++ show bool
synPrintMultiplier indent (Syn.MultNot mulr) = do
    printIndented indent "MultNot"
    synPrintMultiplier (indent+1) mulr

synPrintSumOpPair :: Int -> (Syn.SumOperation, Syn.Multiplication) -> IO ()
synPrintSumOpPair indent (op, multn) = do
    printIndented indent (show op)
    synPrintMultiplication indent multn

synPrintMultiplication :: Int -> Syn.Multiplication -> IO ()
synPrintMultiplication indent (Syn.Multiplication mulr opPairs) = do
    printIndented indent "Multiplication"
    synPrintMultiplier (indent + 1) mulr
    forM_ opPairs (synPrintMultOpPair (indent + 1))

synPrintLogOpPair :: Int -> (Syn.LogOperation, Syn.Summation) -> IO ()
synPrintLogOpPair indent (op, sumt) = do
    printIndented indent (show op)
    synPrintSummation indent sumt

synPrintSummation :: Int -> Syn.Summation -> IO ()
synPrintSummation indent (Syn.Summation multn opPairs) = do
    printIndented indent "Summation"
    synPrintMultiplication (indent + 1) multn
    forM_ opPairs (synPrintSumOpPair (indent + 1))

synPrintExpr :: Int -> Syn.Expression -> IO ()
synPrintExpr indent (Syn.Expression sumt opPairs) = do
    printIndented indent "Expression"
    synPrintSummation (indent + 1) sumt
    forM_ opPairs (synPrintLogOpPair (indent + 1))

synPrintStatement :: Int -> Syn.Statement -> IO ()
synPrintStatement indent (Syn.StmtCompound stmts) = do
    printIndented indent "StmtCompound"
    forM_ stmts (synPrintStatement (indent + 1))
synPrintStatement indent (Syn.StmtAssignment ident expr) = do
    printIndented indent $ "StmtAssignment " ++ ident
    synPrintExpr (indent + 1) expr
synPrintStatement indent (Syn.StmtCondition expr thenBranch maybeElseBranch) = do
    printIndented indent "StmtCondition"
    synPrintExpr (indent + 1) expr
    synPrintStatement (indent + 1) thenBranch
    case maybeElseBranch of
        Just elseBranch -> synPrintStatement (indent + 1) elseBranch
        Nothing         -> return ()

synPrintBlock :: Int -> Syn.Block -> IO ()
synPrintBlock indent (Syn.BlockDecl idents typ) =
    printIndented indent $ "BlockDecl " ++ show idents ++ show typ
synPrintBlock indent (Syn.BlockStmt stmt) = do
    printIndented indent "BlockStmt"
    synPrintStatement (indent + 1) stmt

synPrintProgram :: Syn.Program -> IO ()
synPrintProgram (Syn.Program blocks) = do
    putStrLn "Program"
    forM_ blocks (synPrintBlock 1)

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
                    synPrintProgram prg

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
