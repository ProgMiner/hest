module Operator where

import Value


data UnaryOperator = UnaryOperator { applyUnaryOperator :: Value -> Value, showUO :: String }
instance Show UnaryOperator where
    show = showUO

data BinaryOperator = BinaryOperator { applyBinaryOperator :: Value -> Value -> Value, showBO :: String }
instance Show BinaryOperator where
    show = showBO

unaryOperatorCannotBeApplied :: String -> Value -> Value
unaryOperatorCannotBeApplied op a = UndefinedValue $ "type error: operator " ++ op ++ " cannot be applied to " ++ show a

binaryOperatorCannotBeApplied :: String -> Value -> Value -> Value
binaryOperatorCannotBeApplied op a b = UndefinedValue $ "type error: operator "
        ++ op ++ " cannot be applied to " ++ show a ++ " and " ++ show b

unaryOperatorBoilerplate :: String -> Value -> Value
unaryOperatorBoilerplate _ a@(UndefinedValue _) = a
unaryOperatorBoilerplate op a = unaryOperatorCannotBeApplied op a

binaryOperatorBoilerplate :: String -> Value -> Value -> Value
binaryOperatorBoilerplate _ a@(UndefinedValue _) _ = a
binaryOperatorBoilerplate _ _ b@(UndefinedValue _) = b
binaryOperatorBoilerplate op a b = binaryOperatorCannotBeApplied op a b
