-- | Implement the multiplication arithmetic operation for expressions (with all of its inherent complexity).
module Expr.Mul
    (
        mul
    )
    where


import Expr (Expr (Atom, Mul))

-- Function for multiplying two expressions
mul :: Expr -> Expr -> Expr

-- Whenever a constant (detected as an Atom with power 0) is multiplied with any other atom we simply multiply the coeff to the latter
-- This implements the special case of multiplying by the identity: 1
-- This is a good indication that this is a powerful and elegant rule since it handles the special case for free
mul (Atom c _ 0) (Atom cy y yp) = Atom (c * cy) y yp
mul x y@(Atom c _ 0) = mul y x      -- Where the second expression is a constant we simply use commutation

-- Whenver the two items being multiplied have the same core expression we simply raise the power and collect the coeff.
mul x@(Atom xc xe xp) y@(Atom yc ye yp)
    | xe == ye = Atom (xc * yc) xe (xp + yp)
    | otherwise = mul2 x y

-- When a product is constructed the colleced coeff should be in the outer Atom and all elements inside the product should have coeff 1


mul2 :: Expr -> Expr -> Expr
mul2 x y = Atom 1 (Mul x y) 1        -- The resulting Mul is encapsulated in an Atom for more efficient pattern-matching
