module Main (main) where

import System.Environment (getArgs, getProgName)

import Stmt (executeProgram)
import Lexer (alexScanTokens)
import Parser (parse)


main :: IO ()
main = do
    args <- getArgs

    case args of
        (filename:_) -> runFile filename
        _ -> ("Usage: " ++) <$> (++ " <filename>") <$> getProgName >>= putStrLn

runFile :: String -> IO ()
runFile filename = do
    file <- readFile filename

    case parse $ alexScanTokens file of
        (Right p) -> executeProgram p
        (Left err) -> print err
