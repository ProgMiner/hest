module Expr where

import Value
import Operator


data Expr
    = BracesExpr Expr
    | UnaryExpr UnaryOperator Expr
    | BinaryExpr BinaryOperator Expr Expr
    | IfThenElseExpr Expr Expr Expr
    | ValueExpr Value
    | NameExpr String
    deriving Show


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
