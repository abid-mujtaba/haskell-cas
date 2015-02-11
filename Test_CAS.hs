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
-- This module provides a test suite for the CAS module.
--
-- To run the tests inside GHCi load the module and then use the 'tests' object as follows:
--
-- > :l TestCAS
-- > runTestTT tests
--
-- Test tests can also be run by compiling the module and executing it. This task is automated in the Makefile so all
-- you need to do is run:
--
-- make test



module Test_CAS
    (
        main,               -- We must export the main function if we want the module to be compilable
        tests,
    )
    where


import Control.Applicative
import Test.HUnit
import Test.QuickCheck

import CAS

main = do
          runTestTT tests          -- In the main function we simply run the tests. So running the executable (TestCAS) will now cause the tests to be executed
          quickTests


-- Define the first 10 positive and 9 negative integers for testing.
z0, z1, z2, z3, z4, z5, z6, z7, z8, z9 :: (Integral a) => Expr a
z0 = const' 0
z1 = const' 1
z2 = const' 2
z3 = const' 3
z4 = const' 4
z5 = const' 5
z6 = const' 6
z7 = const' 7
z8 = const' 8
z9 = const' 9

zm1, zm2, zm3, zm4, zm5, zm6, zm7, zm8, zm9 :: (Integral a) => Expr a
zm1 = const' (-1)
zm2 = const' (-2)
zm3 = const' (-3)
zm4 = const' (-4)
zm5 = const' (-5)
zm6 = const' (-6)
zm7 = const' (-7)
zm8 = const' (-8)
zm9 = const' (-9)


-- We add a label to the TestCase by using the TestLabel constructor.
-- The assertions are grouped together as a single TestCase
-- Since the assertions are IO () we use the 'do' keyword to group together a sequence of them
-- The first assertion that fails causes the entire TestCase to fail and the subsequent assertions are not tested
-- Each assertEqual call takes the format: aE <failure message> <expected value> <actual/tested value>

tests = TestList [                                              -- We create a list of TestCases

            TestLabel "Comparing Constants" $                     -- We use TestLabel to add a label to the TestCase which will be shown in case of failure
                TestCase $ do                                   -- Each TestCase contains a sequence of assertions inside a do construct

                    aE "test1" z2 z2                            -- If this assertion fails both "Testing Constants" and "test1" will appear in the report
                    aE "test2" zm3 zm3

                    aB "test3" $ z4 > z3
                    aB "test4" $ z5 < z6
                    aB "test5" $ zm3 < zm2
                    aB "test6" $ zm7 > zm8
                    aB "test7" $ z6 > zm5
                    aB "test8" $ z6 > zm6
            ,                                                   -- This comma delimits the TestLabels inside the TestList list

            TestLabel "Adding Constants" $
                TestCase $ do

                    aE "test1" z5 (z2 + z3)
                    aE "test2" z4 (z7 + zm3)
                    aE "test3" zm4 (zm4 + z0)
            ,

            TestLabel "Multiplying Constants" $
                TestCase $ do

                    aE "test1" z6 (z2 * z3)
                    aE "test2" z0 (z0 * z9)
                    aE "test3" z8 (zm2 * zm4)
                    aE "test4" z0 (zm9 * z0)
                    aE "test5" zm6 (z2 * zm3)
                    aE "test6" z7 (z1 * z7)
                    aE "test7" zm9 (zm9 * z1)
                    aE "test8" zm7 (z7 * zm1)

        ]



-- Define shorthand utility functions for assertions

aE :: (Eq a, Show a) => String -> a -> a -> Assertion
aE = assertEqual

aB :: String -> Bool -> Assertion
aB = assertBool


-- We define a composite IO action consisting of all quickCheck property tests defined in the module
-- If one wants a look at the generated expressions in any quickCheck simply replace the call with 'verboseCheck'. This is a good debugging strategy.

quickTests = do
                quickCheck prop_Add_0
                quickCheck prop_Mul_1


-- Since Expr is a custom class we must make it an instance of the Arbitrary type-class before we can use it inside QuickCheck properties. The instantiation will let QuickCheck know how to generate random objects of type Expr
-- 'arbitrary' is a definition (it is a function that takes no arguments so it is in effect a constant) which in this context must be of type 'Gen (Expr a)' i.e. an IO which corresponds to a random expression.
-- We define it using the 'sized' function which takes as its single argument a function taking an integer and returning a Gen (Expr a)
-- When we use 'sized' we get access to the size integer that QuickCheck uses to create arbitrary instances. We can use this size value to more intelligently construct the expressions (which is the purpose of arbitrary')

instance Integral a => Arbitrary (Expr a) where
  arbitrary = sized arbitrary'


arbitrary' :: Integral a => Int -> Gen (Expr a)
arbitrary' 0 = oneof [return $ const' 0, return $ const' 1]     -- Base case which we make equal to the additive and multiplicative identities
arbitrary' 1 = oneof [arbitrary_atom, arbitrary_neg_atom]       -- When the required size is 1 we simply return an atomic expression (which can be negative)
arbitrary' n = (*) <$> (pure x) <*> arbitrary' (n - 1)

-- For the non-base case we do a little test to see if we can get multiplication going in the arbitrary expression construction.
-- The original definition for this was:
--                                                  = fmap (x*) $ arbitary' (n - 1)
--
-- where the idea was that we treat (x*) as a function of a single argument (since it is curried with x supplied) and use fmap to apply the function inside the Gen (Expr a) context returned by "arbitrary' (n - 1)".
--
-- We then replaced this definition with:
--                                                  = (x*) <$> arbitrary' (n - 1)
--
-- where Control.Applicative module provides the <$> infix operator (function) that simplifies and codifies this common pattern. We place the function to be applied on its left and the contextuallized argument on the right and <$> maps the function inside the context.
--
-- We are now at our next step of the evolution where we want to sandwich the multiplication operator between two objects which are both inside the Gen context. To that end we use both the <$> and the <*> infix operators.
-- <$> takes the (*) two-arg function and maps it over (pure x) giving us (x*) inside the Gen context.
-- We then use <*> to apply the contextualized function (x*) to the contextualized result of arbitrary' (n - 1)
--
-- Note: This definition is equivalent to:
--                                              pure (*) <*> (pure x) <*> arbitrary' (n - 1)
--
-- where we take (*) and use 'pure' to put it in the minimal context and now we can use <*> rather than <$> to apply the function to the two contextualized arguments.
--
-- Note that (pure x) simply places x in the minimal Gen context (that is the purpose of the pure function as it is defined for applicative functors).



-- Constants and Symbols are the atomic expressions. Everything else is constructed from these (or by encapsulatng them in some fashion).
-- We collect the Const and Symbol expression in to a single arbitrary definition which produces them with equal likelihood
-- This definition will be used to create negative atomic expressions as well.
arbitrary_atom :: Integral a => Gen (Expr a)
arbitrary_atom = oneof [arbitrary_const, arbitrary_symbol]

-- arbitrary_const returns a random Const object by taking a random integer from 0 to 9 and wrapping it inside Const.
-- Negative constants are handled by 'arbitrary_negative' which takes positive constants and negates them.
arbitrary_const :: Integral a => Gen (Expr a)
arbitrary_const = fmap const' $ elements ([0, 1 .. 9] :: [Int])

-- The constraint 'Integral a' in the signature is crucial since it allows us to use the const' smart constructor to create Const objects from randomly selected Int.

-- An alternate definition could be:
--
--        = do
--            n <- elements ([0,1..9] :: [Int])
--            return $ const' n
--
-- This alternate definition shows how one can extract a random integer from its 'Gen' context then construct a Const with it using const' and place it back in a Gen context with return. This is however equivalent to wanting to apply the function (constructor) const' to the random integer inside the Gen context and have the result remain inside the context. This is the exact purpose of 'fmap'.

-- We simply use fmap to apply const' inside the Gen (random) context to get a random Const which is what we want.


-- Analogous to 'arbitrary_const' this definition, 'arbitrary_symbol', returns a Symbol object corresponding (randomly) to x, y or z.
-- Note that the signature reveals this to be a definition (and not a function). It corresponds to a randomly selected Expr.
arbitrary_symbol :: Gen (Expr a)
arbitrary_symbol = fmap Symbol $ elements ["x", "y", "z"]


-- This definition creates randomly generated negative atomic expressions
arbitrary_neg_atom :: Integral a => Gen (Expr a)
arbitrary_neg_atom = fmap negate arbitrary_atom     -- We map the negate function on the expression inside the Gen returned by arbitrary_atom



prop_Add_0 :: Expr Int -> Bool      -- A property of expressions is that adding zero to an expression should result in the same expression
prop_Add_0 e = e + z0 == e
    where types = e::(Expr Int)

prop_Mul_1 :: Expr Int -> Bool
prop_Mul_1 e = e * z1 == e
    where types = e::(Expr Int)