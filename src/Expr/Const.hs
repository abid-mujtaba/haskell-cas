-- | Module that defines the behaviour of constants in the CAS
module Expr.Const
    (
        fromInteger
    )
    where

import Prelude hiding (fromInteger)      -- Hide fromInteger since we are defining a function with this very name in this module

import Expr (Expr(Atom, Symbol))

-- We define a utility method for creating constant expressions. It takes an integer and creates a constant expression by creating an Atom with the constant as the coefficient, the core is the special symbol with an empty strings and its power is zero since any symbol to the power zero is equal to 1. The way to detect (pattern match) explicitly for a constant is to look for an Atom with power 0 since any core rasied to the power 0 is by definition equal to 1 and so is a constant
fromInteger :: Integer -> Expr
fromInteger c = Atom c (Symbol "") 0

-- We are using Symbol "" instead of a dedicated constructor (e.g. Const) since we want to treat constants as just a special case of Atom which will allow us to deal with them like any other Atom without having to write a special pattern in every function to deal with them. This was the entire reason behind creating an Atom constructor containing both coefficient and power and forcing everything to be an Atom
