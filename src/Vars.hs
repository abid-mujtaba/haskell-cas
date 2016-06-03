-- | This module declares useful variables (expressions) which are used in ghci to aid in debugging and development.
module Vars (
    x, y, z,
) where

import Prelude hiding ((^))             -- We have to suppress the Prelude definition of (^) since it conflicts with the one in CAS
import CAS

-- | We define and export some useful symbols (the ubiquitous x, y, z).
x, y, z :: Expr a
x = Symbol "x"
y = Symbol "y"
z = Symbol "z"

-- To create the variables we simply use the 'Symbol String' constructor.
