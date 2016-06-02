-- Define all quicktests that are used to test the additive properties of expresssions

module QuickTests.Addition (tests)
    where

import Test.QuickCheck (quickCheck, counterexample)

import CAS

import QuickTests.Arbitrary (arbitrary)


-- If one wants a look at the generated expressions in any quickCheck simply replace the call with 'verboseCheck'. This is a good debugging strategy.
-- We use 'counterexample' to attach a label to each test which will be printed if the test fails. This will let us know at a glance what went wrong.

tests = do
            quickCheck $ counterexample "Adding zero to an expression" prop_Add_0
            quickCheck $ counterexample "Adding an expression with itself" prop_Add_equal
            quickCheck $ counterexample "Add a constant times an expression with another contant times its expressions" prop_Add_equal2
            quickCheck $ counterexample "Subtract an expression from itself" prop_Sub_equal
            quickCheck $ counterexample "Additive Commutation between two expressions" prop_Add_Commute


-- Any function that starts with "prop_" is considered a property by QuickCheck
-- Define properties that test the additive features of expressions

prop_Add_0 :: Expr Int -> Bool      -- A property of expressions is that adding zero to an expression should result in the same expression
prop_Add_0 e = e + 0 == e
    where types = e::(Expr Int)

prop_Add_equal :: Expr Int -> Bool
prop_Add_equal e = e + e == (2 * e)
    where types = e :: (Expr Int)

prop_Add_equal2 :: Expr Int -> Bool
prop_Add_equal2 e = (2 * e) + (5 * e) == (7 * e)
    where types = e :: (Expr Int)

prop_Sub_equal :: Expr Int -> Bool
prop_Sub_equal e = (e - e) == 0
    where types = e :: (Expr Int)

prop_Add_Commute :: Expr Int -> Expr Int -> Bool
prop_Add_Commute e1 e2 = e1 + e2 == e2 + e1
    where types = (e1 :: Expr Int, e2 :: Expr Int)
