module Main where

import Lib


binaryOperatorBoilerplate :: String -> Value -> Value -> Value
binaryOperatorBoilerplate _ a@(UndefinedValue _) _ = a
binaryOperatorBoilerplate _ _ b@(UndefinedValue _) = b
binaryOperatorBoilerplate op a b = binaryOperatorCannotBeApplied op a b

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
main = do
    putStrLn $ show expr1
    putStrLn $ show $ evaluateExpr expr1

    putStrLn $ show expr2
    putStrLn $ show $ evaluateExpr expr2

    putStrLn $ show expr3
    putStrLn $ show $ evaluateExpr expr3
