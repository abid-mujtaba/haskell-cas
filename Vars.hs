-- We declare useful symbols and constants here which we will use in both ghci and the Test module

module Vars (
    x, y, z,
) where

import Prelude hiding ((^))             -- We have to suppress the Prelude definition of (^) since it conflicts with the one in CAS
import CAS

-- We define and export some useful symbols which will save us time later when we export the module. To that end we simply use the 'Symbol String' constructor.

x, y, z :: Expr a
x = Symbol "x"
y = Symbol "y"
z = Symbol "z"