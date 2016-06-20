-- | Implement the multiplication arithmetic operation for expressions (with all of its inherent complexity).
module Expr.Mul
    (
        mul
    )
    where


import Expr (Expr (Atom, Mul))

mul :: Expr -> Expr -> Expr
mul x y = Atom 1 (Mul x y) 1        -- The resulting Mul is encapsulated in an Atom for more efficient pattern-matching
