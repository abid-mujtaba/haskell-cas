-- Defines objects that are used by ALL the sub-modules of UnitTests

module UnitTests.Base
    where

import Test.HUnit(assertEqual, assertBool, Assertion)

import CAS

-- Define variables (expressions) that are used in all of the tests below
-- The variables are defined using the 'symbol' utility function defined in Expr and exported by CAS
x, y, z :: Expr
x = symbol "x"
y = symbol "y"
z = symbol "z"


-- Define shorthand utility functions for assertions
aE :: (Eq a, Show a) => String -> a -> a -> Assertion
aE = assertEqual

aB :: String -> Bool -> Assertion
aB = assertBool
