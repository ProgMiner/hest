module Token where

import Value


data Token
    = NameToken String
    | ValueToken Value
    | LetEqToken
    | IfToken
    | ThenToken
    | ElseToken
    | LBraceToken
    | RBraceToken
    | OpToken String
    deriving Show
