-- | The Expr.Show module makes the Expr type an instance of the Show class allowing expressions to be printed (shown)
module Expr.Show
    (
        show
    )
    where


import Expr (Expr (Symbol, Atom))           -- We need access to ALL constructors of Expr since we need to pattern match against them

-- We declare Expr to be an instance of the Show type-class
-- This requires that we define the 'show' function and what it is supposed to do when acting on an object of type Expr
instance Show Expr where
    show (Symbol s) = s                                                 -- Inside every Symbol is a String. We simply tell 'show' that for a Symbol object simply show the String inside
    show (Atom c e p) = show c ++ " " ++ show e ++ "^" ++ show p        -- We use String concatenation to define how an atom is to be represented as a String
