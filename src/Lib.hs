module Lib where

import Data.Maybe (isJust, isNothing, fromJust)
import Control.Applicative ((<|>))
import Data.List.Extra (notNull)
import Data.List (sort, group)
import Text.Read (readMaybe)


data UnaryOperator = UnaryOperator { applyUnaryOperator :: Value -> Value, showUO :: String }
instance Show UnaryOperator where
    show = showUO

data BinaryOperator = BinaryOperator { applyBinaryOperator :: Value -> Value -> Value, showBO :: String }
instance Show BinaryOperator where
    show = showBO

data Value
    = IntegerValue Integer
    | DoubleValue Double
    | BoolValue Bool
    | UndefinedValue String
    deriving Show

data Expr
    = BracesExpr Expr
    | UnaryExpr UnaryOperator Expr
    | BinaryExpr BinaryOperator Expr Expr
    | IfThenElseExpr Expr Expr Expr
    | ValueExpr Value
    | NameExpr String
    deriving Show

data Stmt = LetStmt String Expr deriving Show
type Program = [Stmt]


evaluateExpr :: [(String, Value)] -> Expr -> Value
evaluateExpr names (BracesExpr e) = evaluateExpr names e
evaluateExpr names (UnaryExpr op e) = applyUnaryOperator op $ evaluateExpr names e
evaluateExpr names (BinaryExpr op a b) = applyBinaryOperator op (evaluateExpr names a) (evaluateExpr names b)
evaluateExpr names (IfThenElseExpr c a b) = case (evaluateExpr names c) of
        (BoolValue c') -> if c' then evaluateExpr names a else evaluateExpr names b
        c'             -> UndefinedValue $ "type error: " ++ show c' ++ " is not BoolValue"

evaluateExpr _     (ValueExpr v) = v
evaluateExpr names (NameExpr name) = case lookup name names of
        Nothing -> UndefinedValue $ "interpretation error: name " ++ name ++ " is not defined"
        (Just v) -> v


executeProgram :: Program -> IO ()
executeProgram program = do
    let namedExprs = analyzeProgramNames program
    let namedExprsDeps = map (fmap analyzeExprDeps) namedExprs

    let externalNames = filter (isNothing . flip lookup namedExprs)
            $ map head $ group $ sort $ concat $ map snd namedExprsDeps

    externalValues <- mapM readValueIO externalNames
    let sortedNames = filter (flip notElem externalNames) $ sortNamesByDeps namedExprsDeps
    let sortedNamedExprs = zip sortedNames $ map (fromJust . flip lookup namedExprs) sortedNames
    let values = foldl (\vs (n, e) -> (n, evaluateExpr vs e):vs) externalValues sortedNamedExprs
    let names = map fst namedExprs

    mapM_ showValueIO $ zip names $ map (fromJust . flip lookup values) names


analyzeProgramNames :: Program -> [(String, Expr)]
analyzeProgramNames = foldr analyzeStmt [] where
    analyzeStmt (LetStmt name expr) result = (name, expr):result

analyzeExprDeps :: Expr -> [String]
analyzeExprDeps (BracesExpr e) = analyzeExprDeps e
analyzeExprDeps (UnaryExpr _ e) = analyzeExprDeps e
analyzeExprDeps (BinaryExpr _ a b) = analyzeExprDeps a ++ analyzeExprDeps b
analyzeExprDeps (IfThenElseExpr c a b) = analyzeExprDeps a ++ analyzeExprDeps b ++ analyzeExprDeps c
analyzeExprDeps (ValueExpr _) = []
analyzeExprDeps (NameExpr name) = [name]

sortNamesByDeps :: [(String, [String])] -> [String]
sortNamesByDeps nodesEdges = reverse $ fst $ sortNamesByDeps' [] $ map fst nodesEdges where
    sortNamesByDeps' :: [String] -> [String] -> ([String], [String])
    sortNamesByDeps' visited []           = ([], visited)
    sortNamesByDeps' visited (node:nodes) = (nodes'' ++ nodes', visited'') where
        (nodes', visited') = sortNamesByDeps'' visited node
        (nodes'', visited'') = sortNamesByDeps' visited' nodes

    sortNamesByDeps'' :: [String] -> String -> ([String], [String])
    sortNamesByDeps'' visited node | node `elem` visited = ([], visited)
                                | isNothing $ lookup node nodesEdges = ([node], node:visited)

    sortNamesByDeps'' visited node = (node:sortedNodes, visited') where
        (sortedNodes, visited') = sortNamesByDeps' (node:visited) $ fromJust $ lookup node nodesEdges


showValueIO :: (String, Value) -> IO ()
showValueIO (name, value) = putStrLn $ name ++ " = " ++ showValue value

showValue :: Value -> String
showValue (IntegerValue v) = show v
showValue (DoubleValue v) = show v
showValue (BoolValue v) = show v
showValue (UndefinedValue msg) = "undefined (" ++ msg ++ ")"

readValueIO :: String -> IO (String, Value)
readValueIO name = ((,) name) <$> putPromptAndRead where
    putPromptAndRead = readValue <$> (putStr (name ++ " = ") >> getLine) >>= putPromptAndRead'
    putPromptAndRead' (UndefinedValue msg) = putStrLn msg >> putPromptAndRead
    putPromptAndRead' v = return v

readValue :: String -> Value
readValue s = v where
    (Just v) =  IntegerValue <$> readMaybe s
            <|> DoubleValue  <$> readMaybe s
            <|> BoolValue    <$> readMaybe s
            <|> Just (UndefinedValue $ "parsing error: cannot parse " ++ show s)


unaryOperatorCannotBeApplied :: String -> Value -> Value
unaryOperatorCannotBeApplied op a = UndefinedValue $ "type error: operator " ++ op ++ " cannot be applied to " ++ show a

binaryOperatorCannotBeApplied :: String -> Value -> Value -> Value
binaryOperatorCannotBeApplied op a b = UndefinedValue $ "type error: operator "
        ++ op ++ " cannot be applied to " ++ show a ++ " and " ++ show b
