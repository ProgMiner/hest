-- # vim:syntax=lex

{
module Lexer (lex) where

import Prelude hiding (lex)

import Value
import Token
}

%wrapper "monad"

$digit = [0-9]
$op = [\~\!\@\#\$\%\^\&\*\=\+\|\:\<\>\/\?\-]

tokens :-

$white+         ;
"--".*          ;
if              { constToken IfToken }
then            { constToken ThenToken }
else            { constToken ElseToken }
=               { constToken LetEqToken }
\(              { constToken LBraceToken }
\)              { constToken RBraceToken }
$op+            { strToken OpToken }
True            { constToken $ ValueToken $ BoolValue True }
False           { constToken $ ValueToken $ BoolValue False }
undefined       { undefinedToken }
$digit+         { strToken $ ValueToken . IntegerValue . read }
$digit*\.$digit+ { strToken $ ValueToken . DoubleValue . read . ('0' :) }
$digit+[eE]$digit+ { strToken $ ValueToken . DoubleValue . read . ('0' :) }
$digit*\.$digit+[eE]$digit+ { strToken $ ValueToken . DoubleValue . read . ('0' :) }
[0-9a-zA-Z_']+  { strToken $ NameToken }

{
data TokenWrapper = TW Token | WEOF

alexEOF = return WEOF

constToken :: Token -> AlexAction TokenWrapper
constToken t _ _ = return $ TW t

strToken :: (String -> Token) -> AlexAction TokenWrapper
strToken f (_, _, _, str) len = return $ TW $ f $ take len str

posStrToken :: (AlexPosn -> String -> Token) -> AlexAction TokenWrapper
posStrToken f (pos, _, _, str) len = return $ TW $ f pos $ take len str

undefinedToken :: AlexAction TokenWrapper
undefinedToken = posStrToken ut' where
    ut' (AlexPn _ l c) _ = ValueToken $ UndefinedValue $ "undefined at " ++ show l ++ ":" ++ show c


lex :: String -> Either String [Token]
lex str = reverse <$> runAlex str (lex' []) where
    lex' ts = alexMonadScan >>= lex'' where
        lex'' (TW t) = lex' (t:ts)
        lex'' WEOF   = return ts
}
