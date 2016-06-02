-- Define all quicktests that are used to test the multiplicative properties of expresssions

module QuickTests.Multiplication (tests)
    where

import Test.QuickCheck (quickCheck, counterexample)

import CAS

import QuickTests.Arbitrary (arbitrary)


tests = do
            quickCheck $ counterexample "Multiplying an expression by one" prop_Mul_1
            quickCheck $ counterexample "Multiplicative Commutation between two expressions" prop_Mul_Commute


-- Define multiplicative properties

prop_Mul_1 :: Expr Int -> Bool
prop_Mul_1 e = e * 1 == e
    where types = e::(Expr Int)

prop_Mul_Commute :: Expr Int -> Expr Int -> Bool
prop_Mul_Commute e1 e2 = e1 * e2 == e2 * e1
    where types = (e1 :: Expr Int, e2 :: Expr Int)
