-- | Implement the multiplication arithmetic operation for expressions (with all of its inherent complexity).
module Expr.Mul
    (
        mul
    )
    where


import Expr (Expr (Mul))

mul :: Expr -> Expr -> Expr
mul x y = Mul x y
