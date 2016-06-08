-- | The Expr module defines the Expr type-class and consequently forms the core of the CAS module
module Expr
    (
        Expr (Symbol, Atom),
        symbol
    )
    where

import Text.Regex.Posix

-- | The Expr type forms the core of the Computer Algebra System.
--   Its strength lies in its recursive definition of what an expression can be.
data Expr =
            Symbol String                       -- A Symbol is an internal data-type corresponding to a single variable
            | Atom Integer (Expr) Integer       -- The work-horse of the Expr class. This type corresponds to any expression with an integer coefficient (possibly negative) and the core expression is raised to a (possibly negative) integer power
            deriving (Eq)                 -- By declaring that Expr derives from the Eq type-class we declare that two Expressions can be compared for equality using a naive comparison where the expressions are matched recursively in their entirety


symbol :: String -> Expr
symbol name
    -- We use guards to test that the string passed to the function is valid
    -- The first guard uses regex which are posix based hence the strange character class names
    | name =~ "^[[:alpha:]][[:alnum:]_]*$"  = Atom 1 (Symbol name) 1
    | otherwise                             = error "Valid symbol strings start with an alphabet and only contain alphabets, numbers and _ (underscore)."
