module Token where


data Token
    = NameToken String
    | ValueToken String
    | LetEqToken
    | IfToken
    | ThenToken
    | ElseToken
    | LBraceToken
    | RBraceToken
    | OpToken String
    deriving Show
