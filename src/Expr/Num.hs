-- | The Expr.Num module makes the Expr type an instance of the Num class allowing expressions to engage in arithmetic operations which form the core of the functionality of a CAS
module Expr.Num
    (
          (+)         -- Explicitly export the + function. Functions not explicitly exported will raise an exception if they are called.
        , fromInteger
    )
    where

import Expr (Expr())
import qualified Expr.Const (fromInteger)
import qualified Expr.Add (add)

instance Num Expr where
    x + y           = Expr.Add.add x y                   -- Addition is handled by a separate module
    x * y           = error "Not implemented yet"
    x - y           = error "Not implemented yet"
    abs x           = error "Not implemented yet"
    signum x        = error "Not implemented yet"
    fromInteger     = Expr.Const.fromInteger             -- The Num class is able to detect when we are about to arithmetically associate an integer with an expression. Once that happens 'fromInteger' is used to convert the Integer in to an expression corresponding to it so that it can be transparently associated with the expression
