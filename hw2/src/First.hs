module First where

data Expr =
    Const Int |
    Add { eLhs :: Expr, eRhs :: Expr } |
    Div { eLhs :: Expr, eRhs :: Expr } |
    Sub { eLhs :: Expr, eRhs :: Expr } |
    Mul { eLhs :: Expr, eRhs :: Expr } |
    Pow { eLhs :: Expr, eRhs :: Expr } deriving (Show)

data ArithmeticError = ZeroDiv | NegativePow deriving (Eq, Show)

eval :: Expr -> Either ArithmeticError Int
eval (Const x) = Right x
eval binary = let rhs = eRhs binary
                  lhs = eLhs binary
              in eval lhs >>= \x -> eval rhs >>= op binary x
              where
                op :: Expr -> Int -> Int -> Either ArithmeticError Int
                op (Add _ _) x y = return $ x + y
                op (Div _ _) x y
                    | y == 0    = Left ZeroDiv
                    | otherwise = return $ x `quot` y
                op (Sub _ _) x y = return $ x - y
                op (Mul _ _) x y = return $ x * y
                op (Pow _ _) x y
                    | y < 0     = Left NegativePow
                    | otherwise = return $ x ^ y
                op _ _ _ = undefined

bin :: Int -> [[Int]]
bin 0 = [[]]
bin n = bin (n - 1) >>= \set -> [0:set, 1:set]
