-- Define quicktests that are used for testing the Ordering properties of Expressions

module QuickTests.Ordering (tests)
    where

import Test.QuickCheck

import CAS

import QuickTests.Arbitrary


tests = do
            quickCheck $ counterexample "Reverse Comparison Test" prop_Rev_Order
            quickCheck $ counterexample "Comparing equal expressions" prop_Compare_Equality
            quickCheck $ counterexample "Comparing unequal expressions" prop_Compare_Inequality


-- Define properties (related to Ordering of expressions) that need to be tested

prop_Rev_Order :: Expr Int -> Expr Int -> Bool
prop_Rev_Order e1 e2 = (compare e1 e2) == (flip $ compare e2 e1)
    where types = (e1 :: Expr Int, e2 :: Expr Int)
          flip LT = GT
          flip GT = LT
          flip EQ = EQ

prop_Compare_Equality :: Expr Int -> Bool
prop_Compare_Equality e = (compare e e) == EQ
    where types = e :: (Expr Int)

prop_Compare_Inequality :: Expr Int -> Expr Int -> Bool
prop_Compare_Inequality e1 e2 = if e1 == e2 then (compare e1 e2) == EQ
                                            else (compare e1 e2) /= EQ
