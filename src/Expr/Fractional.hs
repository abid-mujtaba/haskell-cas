-- | Makes Expr an instance of the Fractional class which gives us access to the (/) division operator.
module Expr.Fractional
    (
        (/)
    )
    where

import Expr (Expr(Atom, Inv))
import Expr.Num             -- Since Fractional is a sub-class of Num, Expr needs to be an instance of Num before it can be an instance of Fractional

instance Fractional Expr where
    (/)          = divide
    fromRational = error "Undefined: Not needed for CAS"

-- Division is simply defined in terms of the 'Inv' constructor which defines expressions raised to the power -1
divide :: Expr -> Expr -> Expr
divide a (Atom 1 _ 0)           = a
divide (Atom 1 _ 0) b           = inverse b
divide a (Atom 1 (Inv b) 1)     = a * b
divide (Atom 1 (Inv a) 1) b     = inverse (a * b)
divide a b                      = a * (inverse b)

-- Encapsulate an expression inside an Inv inside an Atom
inverse :: Expr -> Expr
inverse e    = Atom 1 (Inv e) 1
