module Types (Expr(..)) where 
data Expr = Var (Int, Int)
            | Var3 (Int, Int, Int)
            | And Expr Expr
            | Or Expr Expr
            | Not Expr
            | Const Bool
    deriving (Show, Eq)

