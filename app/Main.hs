module Main (main) where

import Prelude hiding (lex)
import System.Environment (getArgs, getProgName)

import Stmt (executeProgram)
import Parser (parse)
import Lexer (lex)


main :: IO ()
main = do
    args <- getArgs

    case args of
        (filename:_) -> runFile filename
        _ -> ("Usage: " ++) <$> (++ " <filename>") <$> getProgName >>= putStrLn

runFile :: String -> IO ()
runFile filename = do
    file <- readFile filename

    case lex file >>= parse of
        (Right p) -> executeProgram p
        (Left err) -> putStrLn err
