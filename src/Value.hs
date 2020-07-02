module Value where


data Value
    = IntegerValue Integer
    | DoubleValue Double
    | BoolValue Bool
    | UndefinedValue String
    deriving Show
