module Main (main) where

import System.Environment (getArgs, getProgName)
import Text.Parsec (parse)

import Stmt (executeProgram)
import Parser (program)


main :: IO ()
main = do
    args <- getArgs

    case args of
        (filename:_) -> runFile filename
        _ -> ("Usage: " ++) <$> (++ " <filename>") <$> getProgName >>= putStrLn

runFile :: String -> IO ()
runFile filename = do
    file <- readFile filename

    case parse program filename file of
        (Right p) -> executeProgram p
        (Left err) -> print err
