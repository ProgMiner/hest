module Main where

import Value
import Operator
import Expr


addExpr = BinaryExpr $ BinaryOperator addExpr' "+" where
    addExpr' (IntegerValue a) (IntegerValue b) = IntegerValue $ a + b
    addExpr' (DoubleValue a)  (IntegerValue b) = DoubleValue  $ a + fromIntegral b
    addExpr' (IntegerValue a) (DoubleValue b)  = DoubleValue  $ fromIntegral a + b
    addExpr' (DoubleValue a)  (DoubleValue b)  = DoubleValue  $ a + b
    addExpr' a                b                = binaryOperatorBoilerplate "+" a b


expr1 = addExpr (ValueExpr $ IntegerValue 123) (ValueExpr $ IntegerValue 456)
expr2 = addExpr (ValueExpr $ DoubleValue 123) (ValueExpr $ IntegerValue 456)
expr3 = addExpr (ValueExpr $ DoubleValue 123) (ValueExpr $ BoolValue True)

main :: IO ()
main = undefined
