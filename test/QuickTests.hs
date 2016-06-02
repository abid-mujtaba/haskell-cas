--  Copyright 2015 Abid Hasan Mujtaba
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
--
--
-- This module contains ALL Quick Tests (ones using randomly generated test data) for this project.
-- It exports a single 'main' function that runs all of the tests defined here-in
--
-- Source for QuickCheck usage: http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html

module QuickTests (main)
    where

import Control.Applicative
import Test.QuickCheck
-- import Debug.Trace(trace,traceShow)

import CAS

import QuickTests.Arbitrary (arbitrary)
import qualified QuickTests.Ordering (tests)


main:: IO ()
main = do               -- This IO Action runs only the property checks
            quickTests
            QuickTests.Ordering.tests

-- We define a composite IO action consisting of all quickCheck property tests defined in the module
-- If one wants a look at the generated expressions in any quickCheck simply replace the call with 'verboseCheck'. This is a good debugging strategy.
-- We use 'counterexample' to attach a label to each test which will be printed if the test fails. This will let us know at a glance what went wrong.

quickTests = do
                quickCheck $ counterexample "Adding zero to an expression" prop_Add_0
                quickCheck $ counterexample "Multiplying an expression by one" prop_Mul_1
                quickCheck $ counterexample "Adding an expression with itself" prop_Add_equal
                quickCheck $ counterexample "Add a constant times an expression with another contant times its expressions" prop_Add_equal2
                quickCheck $ counterexample "Subtract an expression from itself" prop_Sub_equal
                quickCheck $ counterexample "Additive Commutation between two expressions" prop_Add_Commute
                quickCheck $ counterexample "Multiplicative Commutation between two expressions" prop_Mul_Commute


-- Define the various properties checked by QuickCheck
-- Any function that starts with "prop_" is considered a property by QuickCheck

prop_Add_0 :: Expr Int -> Bool      -- A property of expressions is that adding zero to an expression should result in the same expression
prop_Add_0 e = e + 0 == e
    where types = e::(Expr Int)

prop_Mul_1 :: Expr Int -> Bool
prop_Mul_1 e = e * 1 == e
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

prop_Mul_Commute :: Expr Int -> Expr Int -> Bool
prop_Mul_Commute e1 e2 = e1 * e2 == e2 * e1
    where types = (e1 :: Expr Int, e2 :: Expr Int)
