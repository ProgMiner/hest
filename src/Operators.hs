module Operators where

import Data.Bits ((.&.), (.|.), xor, shift, complement)

import Value
import Operator


-- Unary operators: -, ~, !, ?

unaryMinusOperator = UnaryOperator unaryMinusOperator' "-" where
    unaryMinusOperator' (IntegerValue a) = IntegerValue $ (-a)
    unaryMinusOperator' (DoubleValue  a) = DoubleValue  $ (-a)
    unaryMinusOperator'               a  = unaryOperatorBoilerplate "-" a

bitwiseNotOperator = UnaryOperator bitwiseNotOperator' "~" where
    bitwiseNotOperator' (IntegerValue a) = IntegerValue $ complement a
    bitwiseNotOperator'               a  = unaryOperatorBoilerplate "~" a

notOperator = UnaryOperator notOperator' "!" where
    notOperator' (BoolValue a) = BoolValue $ not a
    notOperator'            a  = unaryOperatorBoilerplate "!" a

unaryElvisOperator = UnaryOperator unaryElvisOperator' "?" where
    unaryElvisOperator' (UndefinedValue _) = BoolValue False
    unaryElvisOperator'                 _  = BoolValue True


-- Binary operators: +, -, *, **, /, //, %, &, |, ^, <<, >>, &&, ||, ?:

addOperator = BinaryOperator addOperator' "+" where
    addOperator' (IntegerValue a) (IntegerValue b) = IntegerValue $              a +              b
    addOperator' (DoubleValue  a) (IntegerValue b) = DoubleValue  $              a + fromIntegral b
    addOperator' (IntegerValue a) (DoubleValue  b) = DoubleValue  $ fromIntegral a +              b
    addOperator' (DoubleValue  a) (DoubleValue  b) = DoubleValue  $              a +              b
    addOperator'               a                b  = binaryOperatorBoilerplate "+" a b

subtractOperator = BinaryOperator subtractOperator' "-" where
    subtractOperator' (IntegerValue a) (IntegerValue b) = IntegerValue $              a -              b
    subtractOperator' (DoubleValue  a) (IntegerValue b) = DoubleValue  $              a - fromIntegral b
    subtractOperator' (IntegerValue a) (DoubleValue  b) = DoubleValue  $ fromIntegral a -              b
    subtractOperator' (DoubleValue  a) (DoubleValue  b) = DoubleValue  $              a -              b
    subtractOperator'               a                b  = binaryOperatorBoilerplate "-" a b

multiplyOperator = BinaryOperator multiplyOperator' "*" where
    multiplyOperator' (IntegerValue a) (IntegerValue b) = IntegerValue $              a *              b
    multiplyOperator' (DoubleValue  a) (IntegerValue b) = DoubleValue  $              a * fromIntegral b
    multiplyOperator' (IntegerValue a) (DoubleValue  b) = DoubleValue  $ fromIntegral a *              b
    multiplyOperator' (DoubleValue  a) (DoubleValue  b) = DoubleValue  $              a *              b
    multiplyOperator'               a                b  = binaryOperatorBoilerplate "*" a b

powerOperator = BinaryOperator powerOperator' "**" where
    powerOperator' (IntegerValue a) (IntegerValue b) = pow'                       a                 b
    powerOperator' (DoubleValue  a) (IntegerValue b) = DoubleValue $              a ** fromIntegral b
    powerOperator' (IntegerValue a) (DoubleValue  b) = DoubleValue $ fromIntegral a **              b
    powerOperator' (DoubleValue  a) (DoubleValue  b) = DoubleValue $              a **              b
    powerOperator'               a                b  = binaryOperatorBoilerplate "**" a b

    pow' :: Integer -> Integer -> Value
    pow' a b | b < 0     = DoubleValue  $ fromIntegral a ** fromIntegral b
             | otherwise = IntegerValue $ a ^ b

divisionOperator = BinaryOperator divisionOperator' "/" where
    divisionOperator' (IntegerValue a) (IntegerValue b) = chkZero a b $ IntegerValue $ quot         a                b
    divisionOperator' (DoubleValue  a) (IntegerValue b) = chkZero a b $ DoubleValue  $              a / fromIntegral b
    divisionOperator' (IntegerValue a) (DoubleValue  b) = chkZero a b $ DoubleValue  $ fromIntegral a /              b
    divisionOperator' (DoubleValue  a) (DoubleValue  b) = chkZero a b $ DoubleValue  $              a /              b
    divisionOperator'               a                b  = binaryOperatorBoilerplate "/" a b

    chkZero :: (Eq a, Num a, Ord a, Eq b, Num b) => a -> b -> Value -> Value
    chkZero a b v | a == 0 && b == 0 = IntegerValue 1
                  | a >  0 && b == 0 = DoubleValue $ 1 / 0
                  |           b == 0 = DoubleValue $ (-1) / 0
                  | otherwise        = v

integralDivisionOperator = BinaryOperator integralDivisionOperator' "//" where
    integralDivisionOperator' (IntegerValue a) (IntegerValue b) = chkZero a b $ IntegerValue $ quot                    a                b
    integralDivisionOperator' (DoubleValue  a) (IntegerValue b) = chkZero a b $ IntegerValue $ truncate $              a / fromIntegral b
    integralDivisionOperator' (IntegerValue a) (DoubleValue  b) = chkZero a b $ IntegerValue $ truncate $ fromIntegral a /              b
    integralDivisionOperator' (DoubleValue  a) (DoubleValue  b) = chkZero a b $ IntegerValue $ truncate $              a /              b
    integralDivisionOperator'               a                b  = binaryOperatorBoilerplate "//" a b

    chkZero :: (Eq a, Num a, Eq b, Num b) => a -> b -> Value -> Value
    chkZero a b v | a == 0 && b == 0 = IntegerValue 1
                  |           b == 0 = UndefinedValue "value error: division by zero"
                  | otherwise        = v

reminderOperator = BinaryOperator reminderOperator' "%" where
    reminderOperator' (IntegerValue a) (IntegerValue b) = chkZero a b $ IntegerValue $ rem                a                b
    reminderOperator' (DoubleValue  a) (IntegerValue b) = chkZero a b $ DoubleValue  $ rem'               a  (fromIntegral b)
    reminderOperator' (IntegerValue a) (DoubleValue  b) = chkZero a b $ DoubleValue  $ rem' (fromIntegral a)               b
    reminderOperator' (DoubleValue  a) (DoubleValue  b) = chkZero a b $ DoubleValue  $ rem'               a                b
    reminderOperator'               a                b  = binaryOperatorBoilerplate "%" a b

    rem' :: Double -> Double -> Double
    rem' a b = a - b * (fromIntegral $ truncate $ a / b)

    chkZero :: (Eq a, Num a, Eq b, Num b) => a -> b -> Value -> Value
    chkZero a b v | a == 0 && b == 0 = IntegerValue 0
                  |           b == 0 = UndefinedValue "value error: division by zero"
                  | otherwise        = v

bitwiseAndOperator = BinaryOperator bitwiseAndOperator' "&" where
    bitwiseAndOperator' (IntegerValue a) (IntegerValue b) = IntegerValue $ a .&. b
    bitwiseAndOperator'               a                b  = binaryOperatorBoilerplate "&" a b

bitwiseOrOperator = BinaryOperator bitwiseOrOperator' "|" where
    bitwiseOrOperator' (IntegerValue a) (IntegerValue b) = IntegerValue $ a .|. b
    bitwiseOrOperator'               a                b  = binaryOperatorBoilerplate "|" a b

bitwiseXorOperator = BinaryOperator bitwiseXorOperator' "^" where
    bitwiseXorOperator' (IntegerValue a) (IntegerValue b) = IntegerValue $ xor a b
    bitwiseXorOperator'               a                b  = binaryOperatorBoilerplate "^" a b

bitwiseRolOperator = BinaryOperator bitwiseRolOperator' "<<" where
    bitwiseRolOperator' (IntegerValue a) (IntegerValue b) = IntegerValue $ shift a $ fromInteger b
    bitwiseRolOperator'               a                b  = binaryOperatorBoilerplate "<<" a b

bitwiseRorOperator = BinaryOperator bitwiseRorOperator' ">>" where
    bitwiseRorOperator' (IntegerValue a) (IntegerValue b) = IntegerValue $ shift a $ fromInteger (-b)
    bitwiseRorOperator'               a                b  = binaryOperatorBoilerplate ">>" a b

andOperator = BinaryOperator andOperator' "&&" where
    andOperator' (BoolValue a) (BoolValue b) = BoolValue $ a && b
    andOperator'            a             b  = binaryOperatorBoilerplate "&&" a b

orOperator = BinaryOperator orOperator' "||" where
    orOperator' (BoolValue a) (BoolValue b) = BoolValue $ a || b
    orOperator'            a             b  = binaryOperatorBoilerplate "||" a b

elvisOperator = BinaryOperator elvisOperator' "?:" where
    elvisOperator' (UndefinedValue _) b = b
    elvisOperator'                 a  _ = a


-- Compare operators: <, >, ==, !=, <=, >=

lowerOperator = BinaryOperator lowerOperator' "<" where
    lowerOperator' (IntegerValue a) (IntegerValue b) = BoolValue $              a <              b
    lowerOperator' (DoubleValue  a) (IntegerValue b) = BoolValue $              a < fromIntegral b
    lowerOperator' (IntegerValue a) (DoubleValue  b) = BoolValue $ fromIntegral a <              b
    lowerOperator' (DoubleValue  a) (DoubleValue  b) = BoolValue $              a <              b
    lowerOperator'               a                b  = binaryOperatorBoilerplate "<" a b

greaterOperator = BinaryOperator greaterOperator' ">" where
    greaterOperator' (IntegerValue a) (IntegerValue b) = BoolValue $              a >              b
    greaterOperator' (DoubleValue  a) (IntegerValue b) = BoolValue $              a > fromIntegral b
    greaterOperator' (IntegerValue a) (DoubleValue  b) = BoolValue $ fromIntegral a >              b
    greaterOperator' (DoubleValue  a) (DoubleValue  b) = BoolValue $              a >              b
    greaterOperator'               a                b  = binaryOperatorBoilerplate ">" a b

equalsOperator = BinaryOperator equalsOperator' "==" where
    equalsOperator' (IntegerValue a) (IntegerValue b) = BoolValue $              a ==              b
    equalsOperator' (DoubleValue  a) (IntegerValue b) = BoolValue $              a == fromIntegral b
    equalsOperator' (IntegerValue a) (DoubleValue  b) = BoolValue $ fromIntegral a ==              b
    equalsOperator' (DoubleValue  a) (DoubleValue  b) = BoolValue $              a ==              b
    equalsOperator' (BoolValue    a) (BoolValue    b) = BoolValue $              a ==              b
    equalsOperator'               a                b  = binaryOperatorBoilerplate "==" a b

notEqualsOperator = BinaryOperator notEqualsOperator' "!=" where
    notEqualsOperator' (IntegerValue a) (IntegerValue b) = BoolValue $              a /=              b
    notEqualsOperator' (DoubleValue  a) (IntegerValue b) = BoolValue $              a /= fromIntegral b
    notEqualsOperator' (IntegerValue a) (DoubleValue  b) = BoolValue $ fromIntegral a /=              b
    notEqualsOperator' (DoubleValue  a) (DoubleValue  b) = BoolValue $              a /=              b
    notEqualsOperator' (BoolValue    a) (BoolValue    b) = BoolValue $              a /=              b
    notEqualsOperator'               a                b  = binaryOperatorBoilerplate "!=" a b

lowerEqualsOperator = BinaryOperator lowerEqualsOperator' "<=" where
    lowerEqualsOperator' (IntegerValue a) (IntegerValue b) = BoolValue $              a <=              b
    lowerEqualsOperator' (DoubleValue  a) (IntegerValue b) = BoolValue $              a <= fromIntegral b
    lowerEqualsOperator' (IntegerValue a) (DoubleValue  b) = BoolValue $ fromIntegral a <=              b
    lowerEqualsOperator' (DoubleValue  a) (DoubleValue  b) = BoolValue $              a <=              b
    lowerEqualsOperator'               a                b  = binaryOperatorBoilerplate "<=" a b

greaterEqualsOperator = BinaryOperator greaterEqualsOperator' ">=" where
    greaterEqualsOperator' (IntegerValue a) (IntegerValue b) = BoolValue $              a >=              b
    greaterEqualsOperator' (DoubleValue  a) (IntegerValue b) = BoolValue $              a >= fromIntegral b
    greaterEqualsOperator' (IntegerValue a) (DoubleValue  b) = BoolValue $ fromIntegral a >=              b
    greaterEqualsOperator' (DoubleValue  a) (DoubleValue  b) = BoolValue $              a >=              b
    greaterEqualsOperator'               a                b  = binaryOperatorBoilerplate ">=" a b
