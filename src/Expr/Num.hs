-- | The Expr.Num module makes the Expr type an instance of the Num class allowing expressions to engage in arithmetic operations which form the core of the functionality of a CAS
module Expr.Num
    (
        (+)         -- Explicitly export the + function. Functions not explicitly exported will raise an exception if they are called.
    )
    where

import Expr (Expr(Add))

instance Num Expr where
    -- ToDo: Implement using separate module and deal with commutation
    x + y           = Add x y
    x * y           = error "Not implemented yet"
    x - y           = error "Not implemented yet"
    abs x           = error "Not implemented yet"
    signum x        = error "Not implemented yet"
    fromInteger c   = error "Not implemented yet"
