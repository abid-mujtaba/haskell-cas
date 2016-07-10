-- | The Expr.Show module makes the Expr type an instance of the Show class allowing expressions to be printed (shown)
module Expr.Show
    (
        show
      , showActual
    )
    where

import Expr (Expr (Symbol, Atom, Add, Mul, Inv))           -- We need access to ALL constructors of Expr since we need to pattern match against them
import qualified Expr.Add (negate)

-- We declare Expr to be an instance of the Show type-class
-- This requires that we define the 'show' function and what it is supposed to do when acting on an object of type Expr
instance Show Expr where
    show (Symbol s) = s                                                 -- Inside every Symbol is a String. We simply tell 'show' that for a Symbol object simply show the String inside
    show (Atom c _ 0)     = show c                            -- A constant is detected when we have an atom with power 0. This pattern detects a constant and renders it by showing simply the coefficient
    show (Atom c e@(Inv _) 1) = show c ++ show e              -- An Inv encapsulated inside an isolated Atom
    show (Atom 1 e 1)     = show e
    show (Atom c e p) = showCoeff c ++ show e ++ showPower p            -- We use String concatenation to define how an atom is to be represented as a String. We define separate functions for dealing with the constant and power parts of an atom
    -- Added expressions are always surrounded by parentheses. Recursively added expressions share a single set of parentheses
    show (Add x y@(Atom c _ _))
        | c >= 0    = "(" ++ showInSum x ++ " + " ++ showInSum y ++ ")"
        | otherwise = "(" ++ showInSum x ++ " - " ++ showInSum (Expr.Add.negate y) ++ ")"       -- If the second argument has negative coeff then we show a minus sign and use negate to flip the sign of the expression before showing it
    -- show (Mul x (Atom 1 (Inv y) 1)) = show x ++ " / (" ++ show y ++ ")"
    show (Mul x y) = show x ++ showInMul y
    show (Inv y)        = " / " ++ showInInv y          -- Isolated Inv

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
showInSum :: Expr -> String
-- If an expression consists of an addition with NO coefficient and no power then it is included in the original set of parentheses defined by 'show (Add x y)'
-- If the coeff of the second term is negative then a minus sign is shown as the connector
showInSum (Atom 1 (Add x y@(Atom c _ _)) 1)
    | c < 0    = showInSum x ++ " - " ++ showInSum (Expr.Add.negate y)     -- Since we are showing a "-" sign we negate the second argument before we show it
    | otherwise = showInSum x ++ " + " ++ showInSum y
showInSum e                     = show e                                  -- Every other kind of expression is then relegated back to the original show for rendering


-- Function for dealing with expressions inside an Inv
showInInv :: Expr -> String
showInInv e@(Atom _ (Mul _ _) _)       = "(" ++ show e ++ ")"           -- Products inside Inv should be surrounded by parentheses
showInInv e                            = show e


-- Function for dealing with expressions inside a Mul
showInMul :: Expr -> String
showInMul (Atom 1 ie@(Inv _) 1)   = show ie                             -- An inverse inside a Mul is shown with no space and rendering passed on to 'show' which deals with Inv properly
showInMul b                       = " " ++ show b


-- Utility function for showing the actual constructor and recursive nature of an expression.
showActual :: Expr -> String
showActual (Symbol s) = s
showActual (Add a b) = "(Add " ++ showActual a ++ " " ++ showActual b ++ ")"
showActual (Mul a b) = "(Mul " ++ showActual a ++ " " ++ showActual b ++ ")"
showActual (Atom c e p) = "(Atom " ++ show c ++ " " ++ showActual e ++ " " ++ show p ++ ")"
showActual (Inv e) = "(Inv " ++ show e ++ ")"
