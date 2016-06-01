-- Defines objects that are used by ALL the sub-modules of UnitTests

module UnitTests.Base
    where

import Test.HUnit(assertEqual, assertBool, Assertion)

import CAS

-- Define variables (expressions) that are used in all of the tests below
x, y, z :: Expr a
x = Symbol "x"
y = Symbol "y"
z = Symbol "z"


-- Define shorthand utility functions for assertions
aE :: (Eq a, Show a) => String -> a -> a -> Assertion
aE = assertEqual

aB :: String -> Bool -> Assertion
aB = assertBool
