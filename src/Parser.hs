module Parser where

import Data.Functor (($>))
import Text.ParserCombinators.Parsec

import Value
import Expr
import Operators
import Stmt


program = spaces >> many stmt <* eof

stmt = letStmt where
    letStmt = do
        name' <- name
        spaces
        char '='
        spaces
        expr' <- expr
        spaces
        return $ LetStmt name' expr'

expr =  try exprBinary
    <|> try exprTerm where

    exprBinary = exprTerm
        `chainl1` try exprBinaryElvis
        `chainr1` try exprBinaryPower
        `chainl1` (try exprBinaryMultiply
               <|> try exprBinaryDivision
               <|> try exprBinaryIntegralDivision
               <|> try exprBinaryReminder)
        `chainl1` (try exprBinaryBitwiseRol
               <|> try exprBinaryBitwiseRor)
        `chainl1` (try exprBinaryAdd
               <|> try exprBinarySubtract)
        `chainl1` (try exprBinaryLower
               <|> try exprBinaryGreater
               <|> try exprBinaryEquals
               <|> try exprBinaryNotEquals
               <|> try exprBinaryLowerEquals
               <|> try exprBinaryGreaterEquals)
        `chainl1` (try exprBinaryAnd
               <|> try exprBinaryOr)
        `chainl1` (try exprBinaryBitwiseAnd
               <|> try exprBinaryBitwiseOr
               <|> try exprBinaryBitwiseXor) where

        exprBinaryAdd              = char   '+'  <* spaces $> BinaryExpr addOperator
        exprBinarySubtract         = char   '-'  <* spaces $> BinaryExpr subtractOperator
        exprBinaryMultiply         = char   '*'  <* spaces $> BinaryExpr multiplyOperator
        exprBinaryPower            = string "**" <* spaces $> BinaryExpr powerOperator
        exprBinaryDivision         = char   '/'  <* spaces $> BinaryExpr divisionOperator
        exprBinaryIntegralDivision = string "//" <* spaces $> BinaryExpr integralDivisionOperator
        exprBinaryReminder         = char   '%'  <* spaces $> BinaryExpr reminderOperator
        exprBinaryBitwiseAnd       = char   '&'  <* spaces $> BinaryExpr bitwiseAndOperator
        exprBinaryBitwiseOr        = char   '|'  <* spaces $> BinaryExpr bitwiseOrOperator
        exprBinaryBitwiseXor       = char   '^'  <* spaces $> BinaryExpr bitwiseXorOperator
        exprBinaryBitwiseRol       = string "<<" <* spaces $> BinaryExpr bitwiseRolOperator
        exprBinaryBitwiseRor       = string ">>" <* spaces $> BinaryExpr bitwiseRorOperator
        exprBinaryAnd              = string "&&" <* spaces $> BinaryExpr andOperator
        exprBinaryOr               = string "||" <* spaces $> BinaryExpr orOperator
        exprBinaryElvis            = string "?:" <* spaces $> BinaryExpr elvisOperator
        exprBinaryLower            = char   '<'  <* spaces $> BinaryExpr lowerOperator
        exprBinaryGreater          = char   '>'  <* spaces $> BinaryExpr greaterOperator
        exprBinaryEquals           = string "==" <* spaces $> BinaryExpr equalsOperator
        exprBinaryNotEquals        = string "!=" <* spaces $> BinaryExpr notEqualsOperator
        exprBinaryLowerEquals      = string "<=" <* spaces $> BinaryExpr lowerEqualsOperator
        exprBinaryGreaterEquals    = string ">=" <* spaces $> BinaryExpr greaterEqualsOperator

    exprTerm =  try exprBraces
            <|> try exprUnary
            <|> try exprIfThenElse
            <|> try exprValue
            <|> try exprName

    exprBraces = do
        char '('
        spaces
        expr' <- expr
        spaces
        char ')'
        return $ BracesExpr expr'

    exprUnary = do
        op <- unaryOperator
        spaces
        expr' <- expr
        return $ UnaryExpr op expr'

    exprIfThenElse = do
        string "if"
        spaces
        cond <- expr
        spaces
        string "then"
        spaces
        a <- expr
        spaces
        string "else"
        spaces
        b <- expr
        return $ IfThenElseExpr cond a b

    exprValue = ValueExpr <$> value
    exprName = NameExpr <$> name

unaryOperator = (unaryOperatorMinus
             <|> unaryOperatorBitwiseNot
             <|> unaryOperatorNot
             <|> unaryOperatorElvis)
             <* spaces where
    
    unaryOperatorMinus      = char '-' $> unaryMinusOperator
    unaryOperatorBitwiseNot = char '~' $> bitwiseNotOperator
    unaryOperatorNot        = char '!' $> notOperator
    unaryOperatorElvis      = char '?' $> unaryElvisOperator

value = (try valueDouble
     <|> try valueInteger
     <|> try valueBool
     <|> try valueUndefined)
     <* spaces where

    valueInteger = IntegerValue <$> read <$> many1 digit
    valueDouble = DoubleValue <$> read <$> (try doubleScientific <|> try doublePlain) where
        doubleScientific = (\i _ f _ e -> '0':i ++ '.':f ++ 'e':e)
                <$> many digit
                <*> char '.'
                <*> many1 digit
                <*> (char 'e' <|> char 'E')
                <*> many1 digit

        doublePlain = (\i _ f -> '0':i ++ '.':f)
                <$> many digit
                <*> char '.'
                <*> many1 digit

    valueBool = BoolValue <$> read <$> (try (string "True") <|> try (string "False"))
    valueUndefined = UndefinedValue <$> ("undefined value at " ++) <$> show <$> (string "undefined" >> getPosition)

name = (:) <$> letter <*> (many (letter <|> char '\'' <|> char '_' <|> digit)) <* spaces
