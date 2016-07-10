-- | This module declares useful variables (expressions) which are used in ghci to aid in debugging and development.
module Vars (
    x, y, z,
) where

import CAS

-- | We define and export some useful symbols (the ubiquitous x, y, z).
x, y, z :: Expr
x = symbol "x"
y = symbol "y"
z = symbol "z"

-- To create the variables we simply use the 'symbol' utility function that is defined in Expr and exported by CAS
-- This function provides the only mechanism for creating Atoms that corresond to single symbols/variables
-- i.e. x = Atom 1 "x" 1 = 1 * x^1
