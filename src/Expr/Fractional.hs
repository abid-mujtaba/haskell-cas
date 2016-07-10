-- | Makes Expr an instance of the Fractional class which gives us access to the (/) division operator.
module Expr.Fractional
    (
        (/)
    )
    where

import Expr (Expr())
import Expr.Num             -- Since Fractional is a sub-class of Num, Expr needs to be an instance of Num before it can be an instance of Fractional

instance Fractional Expr where
    x / y = error "Division is undefined"
    fromRational = error "Undefined: Not needed for CAS"
