-- | The Expr.Num module makes the Expr type an instance of the Num class allowing expressions to engage in arithmetic operations which form the core of the functionality of a CAS

-- Source: https://www.haskell.org/tutorial/numbers.html (comprehensive tutorial on Haskell numeric types)

module Expr.Num
    (
          (+)         -- Explicitly export the + function. Functions not explicitly exported will raise an exception if they are called.
        , (*)
        , (-)
        , fromInteger
        , negate
    )
    where

import Expr (Expr())
import qualified Expr.Const (fromInteger)
import qualified Expr.Add (add, negate)
import qualified Expr.Mul (mul)

instance Num Expr where
    x + y           = Expr.Add.add x y                   -- Addition is handled by a separate module
    x * y           = Expr.Mul.mul x y
    x - y           = x + negate y                       -- Subtraction is simply addition with the second expression negated
    abs x           = error "Not implemented yet"
    signum x        = error "Not implemented yet"
    fromInteger     = Expr.Const.fromInteger             -- The Num class is able to detect when we are about to arithmetically associate an integer with an expression. Once that happens 'fromInteger' is used to convert the Integer in to an expression corresponding to it so that it can be transparently associated with the expression
    negate          = Expr.Add.negate

-- Note: Since we are overloading existing arithmetic operators with established order of precedence we get this order for free for our expressions

-- Note: By implementing (*) we get the implementation for (^) for free since it is defined in the Real type-class using multiplication for all instances of the Num class i.e. the (^) method has a uniform implementaion that only uses those aspects of an object that are defined for Num
