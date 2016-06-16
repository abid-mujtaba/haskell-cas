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
    show (Atom c _ 0)     = show c                            -- A constant is detected when we have an atom with power 0. This pattern detects a constant and renders it by showing simply the coefficient
    show (Atom 1 e 1) = show e                                          -- If the atom has coeff 1 and power 1 then simply show the core expr
    show (Atom 1 e p) = show e ++ "^" ++ show p                         -- If the atom has coeff 1 then do not display the coeff
    show (Atom c e p) = show c ++ " " ++ show e ++ "^" ++ show p        -- We use String concatenation to define how an atom is to be represented as a String
    show (Add x y)    = "(" ++ showSum x ++ " + " ++ showSum y ++ ")"   -- Added expressions are always surrounded by parentheses. Recursively added expressions share a single set of parentheses


-- Function for dealing with recursively added expressions
showSum :: Expr -> String
showSum (Atom 1 (Add x y) 1)  = showSum x ++ " + " ++ showSum y         -- If an expression consists of an addition with NO coefficient and no power then it is included in the original set of parentheses defined by 'show (Add x y)'
showSum e                     = show e                                  -- Every other kind of expression is then relegated back to the original show for rendering
