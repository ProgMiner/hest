-- # vim:syntax=yacc

{
module Parser (parse) where

import Token
import Value
import Expr
import Stmt
import Operators
}

%name parse
%tokentype { Token }
%error { parsingError }
%monad { Either String } { (>>=) } { return }

%token
    '='     { LetEqToken }
    if      { IfToken }
    then    { ThenToken }
    else    { ElseToken }
    '('     { LBraceToken }
    ')'     { RBraceToken }
    value   { ValueToken $$ }
    name    { NameToken $$ }
    '-'     { OpToken "-" }
    '~'     { OpToken "~" }
    '!'     { OpToken "!" }
    '?'     { OpToken "?" }
    '+'     { OpToken "+" }
    '*'     { OpToken "*" }
    '**'    { OpToken "**" }
    '/'     { OpToken "/" }
    '//'    { OpToken "//" }
    '%'     { OpToken "%" }
    '&'     { OpToken "&" }
    '|'     { OpToken "|" }
    '^'     { OpToken "^" }
    '<<'    { OpToken "<<" }
    '>>'    { OpToken ">>" }
    '&&'    { OpToken "&&" }
    '||'    { OpToken "||" }
    '?:'    { OpToken "?:" }
    '<'     { OpToken "<" }
    '>'     { OpToken ">" }
    '=='    { OpToken "==" }
    '!='    { OpToken "!=" }
    '<='    { OpToken "<=" }
    '>='    { OpToken ">=" }

%left '&&' '||'
%left '==' '!='
%nonassoc '<' '<=' '>=' '>'
%left '+' '-'
%left '<<' '>>'
%left '&' '|' '^'
%left '*' '/' '//' '%'
%right '**'
%left '?:'
%nonassoc NOT
%left NEG
%nonassoc BNOT
%nonassoc UELV

%%

program
    : stmts         { reverse $1 }

stmts
    : {- empty -}   { [] }
    | stmts stmt    { $2 : $1 }

stmt
    : name '=' expr { LetStmt $1 $3 }

expr
    : '(' expr ')'  { BracesExpr $2 }
    | binaryExpr    { $1 }
    | unaryExpr     { $1 }
    | if expr then expr else expr { IfThenElseExpr $2 $4 $6 }
    | value         { ValueExpr $1 }
    | name          { NameExpr $1 }

unaryExpr
    : '?' expr %prec UELV   { UnaryExpr unaryElvisOperator $2 }
    | '~' expr %prec BNOT   { UnaryExpr bitwiseNotOperator $2 }
    | '-' expr %prec NEG    { UnaryExpr unaryMinusOperator $2 }
    | '!' expr %prec NOT    { UnaryExpr notOperator $2 }

binaryExpr
    : expr '?:' expr    { BinaryExpr elvisOperator $1 $3 }
    | expr '**' expr    { BinaryExpr powerOperator $1 $3 }
    | expr '*'  expr    { BinaryExpr multiplyOperator $1 $3 }
    | expr '/'  expr    { BinaryExpr divisionOperator $1 $3 }
    | expr '//' expr    { BinaryExpr integralDivisionOperator $1 $3 }
    | expr '%'  expr    { BinaryExpr reminderOperator $1 $3 }
    | expr '<<' expr    { BinaryExpr bitwiseRolOperator $1 $3 }
    | expr '>>' expr    { BinaryExpr bitwiseRorOperator $1 $3 }
    | expr '&'  expr    { BinaryExpr bitwiseAndOperator $1 $3 }
    | expr '|'  expr    { BinaryExpr bitwiseOrOperator $1 $3 }
    | expr '^'  expr    { BinaryExpr bitwiseXorOperator $1 $3 }
    | expr '+'  expr    { BinaryExpr addOperator $1 $3 }
    | expr '-'  expr    { BinaryExpr subtractOperator $1 $3 }
    | expr '<'  expr    { BinaryExpr lowerOperator $1 $3 }
    | expr '>'  expr    { BinaryExpr greaterOperator $1 $3 }
    | expr '<=' expr    { BinaryExpr lowerEqualsOperator $1 $3 }
    | expr '>=' expr    { BinaryExpr greaterEqualsOperator $1 $3 }
    | expr '==' expr    { BinaryExpr equalsOperator $1 $3 }
    | expr '!=' expr    { BinaryExpr notEqualsOperator $1 $3 }
    | expr '&&' expr    { BinaryExpr andOperator $1 $3 }
    | expr '||' expr    { BinaryExpr orOperator $1 $3 }

{
parsingError ts = Left $ "Parsing error at " ++ show ts
}
