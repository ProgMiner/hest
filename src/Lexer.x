-- # vim:syntax=lex

{
module Lexer (alexScanTokens) where

import Value
import Token
}

%wrapper "basic"

$digit = [0-9]
$op = [\~\!\@\#\$\%\^\&\*\=\+\|\:\<\>\/\?\-]

tokens :-

$white+         ;
"--".*          ;
if              { const IfToken }
then            { const ThenToken }
else            { const ElseToken }
=               { const LetEqToken }
\(              { const LBraceToken }
\)              { const RBraceToken }
$op+            { OpToken }
True            { const $ ValueToken $ BoolValue True }
False           { const $ ValueToken $ BoolValue False }
undefined       { const $ ValueToken $ UndefinedValue "undefined" }
$digit+         { ValueToken . IntegerValue . read }
$digit*\.$digit+ { ValueToken . DoubleValue . read . ('0' :) }
$digit+[eE]$digit+ { ValueToken . DoubleValue . read . ('0' :) }
$digit*\.$digit+[eE]$digit+ { ValueToken . DoubleValue . read . ('0' :) }
[0-9a-zA-Z_']+  { NameToken }
