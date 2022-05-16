module FunctionSVG (Expr (..), evaluate, evaluateList) where
-- (Expr (..), evaluate)
data Expr = Num Double | X | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Pow Expr Expr | Div Expr Expr
                           | Sin Expr | Cos Expr | Tan Expr
                                                             deriving (Show, Read)

fromExpr :: Expr -> Maybe Double
fromExpr (Num a) = Just a
fromExpr x = Nothing

deepmap :: (Expr -> Expr) -> Expr -> Expr
deepmap f (Num a) = Num a
deepmap f X = X
deepmap f (Add a b) = Add (f a) (f b)
deepmap f (Sub a b) = Sub (f a) (f b)
deepmap f (Mul a b) = Mul (f a) (f b)
deepmap f (Pow a b) = Pow (f a) (f b)
deepmap f (Div a b) = Div (f a) (f b)
deepmap f (Sin a) = Sin (f a)
deepmap f (Cos a) = Cos (f a)
deepmap f (Tan a) = Tan (f a)

replace :: Double -> Expr -> Expr
replace d X = Num d
replace d e = deepmap (replace d) e

count :: Expr -> Maybe Double
count = fromExpr . count' where
  count' (Num a) = Num a
  count' (Add (Num a) (Num b)) = Num (a+b)
  count' (Sub (Num a) (Num b)) = Num (a-b)
  count' (Mul (Num a) (Num b)) = Num (a*b)
  count' (Pow (Num a) (Num b)) = Num (a**b)
  count' (Div (Num a) (Num b)) = Num (a/b)

  count' (Sin (Num a)) = Num (sin a)
  count' (Cos (Num a)) = Num (cos a)
  count' (Tan (Num a)) = Num (tan a) 
  count' e = count' (deepmap count' e)
  
evaluate :: Double -> Expr -> Maybe Double
evaluate d e = count $ replace d e

evaluateList :: Expr -> [Double] -> [Maybe Double]
evaluateList e = foldl (\acc x -> evaluate x e : acc) []