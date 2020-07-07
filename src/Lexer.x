-- # vim:syntax=lex

{
module Lexer where

import Token
}

%wrapper "basic"

$digit = [0-9]
$op = [\~\!\@\#\$\%\^\&\*\=\+\|\:\<\>\/\?\-]

tokens :-

$white+     ;
"--".*      ;
if          { const IfToken }
then        { const ThenToken }
else        { const ElseToken }
=           { const LetEqToken }
\(          { const LBraceToken }
\)          { const RBraceToken }
$op+        { OpToken }
True        { ValueToken }
False       { ValueToken }
undefined   { ValueToken }
$digit+     { ValueToken }
$digit*\.$digit+ { ValueToken }
$digit+[eE]$digit+ { ValueToken }
$digit*\.$digit+[eE]$digit+ { ValueToken }
[0-9a-zA-Z_']+ { NameToken }
