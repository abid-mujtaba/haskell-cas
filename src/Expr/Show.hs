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
    show (Atom c e p) = showCoeff c ++ show e ++ showPower p            -- We use String concatenation to define how an atom is to be represented as a String. We define separate functions for dealing with the constant and power parts of an atom
    show (Add x y)    = "(" ++ showSum x ++ " + " ++ showSum y ++ ")"   -- Added expressions are always surrounded by parentheses. Recursively added expressions share a single set of parentheses


-- Define how to show the coefficient of an Atom is to be rendered/shown
showCoeff :: Integer -> String
showCoeff 1     = ""                    -- A coeff of 1 is not shown
showCoeff (-1)  = "-"                   -- A coeff of -1 is simply shown with a minus sign
showCoeff c     = show c ++ " "         -- The coeff is separated from the next part by a single space


-- Define how the power of an Atomj is to be rendered
showPower :: Integer -> String
showPower 1     = ""                    -- A power of 1 is not shown
showPower p     = "^" ++ show p


-- Function for dealing with recursively added expressions
showSum :: Expr -> String
showSum (Atom 1 (Add x y) 1)  = showSum x ++ " + " ++ showSum y         -- If an expression consists of an addition with NO coefficient and no power then it is included in the original set of parentheses defined by 'show (Add x y)'
showSum e                     = show e                                  -- Every other kind of expression is then relegated back to the original show for rendering
