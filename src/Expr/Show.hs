-- | The Expr.Show module makes the Expr type an instance of the Show class allowing expressions to be printed (shown)
module Expr.Show
    (
        show
    )
    where


import Expr (Expr (Symbol, Atom, Add))           -- We need access to ALL constructors of Expr since we need to pattern match against them

-- We declare Expr to be an instance of the Show type-class
-- This requires that we define the 'show' function and what it is supposed to do when acting on an object of type Expr
instance Show Expr where
    show (Symbol s) = s                                                 -- Inside every Symbol is a String. We simply tell 'show' that for a Symbol object simply show the String inside
    show (Atom 1 e 1) = show e                                          -- If the atom has coeff 1 and power 1 then simply show the core expr
    show (Atom 1 e p) = show e ++ "^" ++ show p                         -- If the atom has coeff 1 then do not display the coeff
    show (Atom c e p) = show c ++ " " ++ show e ++ "^" ++ show p        -- We use String concatenation to define how an atom is to be represented as a String
    -- ToDo: Deal with recursive Add structures
    show (Add x y)    = "(" ++ show x ++ " + " ++ show y ++ ")"         -- Added expressions are always surrounded by parentheses
