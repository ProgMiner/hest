-- # vim:syntax=lex

{
module Lexer (alexScanTokens) where

import Value
import Token
}

%wrapper "posn"

$digit = [0-9]
$op = [\~\!\@\#\$\%\^\&\*\=\+\|\:\<\>\/\?\-]

tokens :-

$white+         ;
"--".*          ;
if              { const $ const IfToken }
then            { const $ const ThenToken }
else            { const $ const ElseToken }
=               { const $ const LetEqToken }
\(              { const $ const LBraceToken }
\)              { const $ const RBraceToken }
$op+            { const $ OpToken }
True            { const $ const $ ValueToken $ BoolValue True }
False           { const $ const $ ValueToken $ BoolValue False }
undefined       { \(AlexPn _ l c) -> const $ ValueToken $ UndefinedValue $ "undefined at " ++ show l ++ ":" ++ show c }
$digit+         { const $ ValueToken . IntegerValue . read }
$digit*\.$digit+ { const $ ValueToken . DoubleValue . read . ('0' :) }
$digit+[eE]$digit+ { const $ ValueToken . DoubleValue . read . ('0' :) }
$digit*\.$digit+[eE]$digit+ { const $ ValueToken . DoubleValue . read . ('0' :) }
[0-9a-zA-Z_']+  { const $ NameToken }
