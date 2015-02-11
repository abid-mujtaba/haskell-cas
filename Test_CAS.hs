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
-- 'arbitrary' is a function which in this context is supposed to return a 'Gen (Expr a)' i.e. an IO which corresponds to a random expression.
-- We define it to be 'oneof' (a random selection) from the list of 'arbitrary' functions which are defined using 'where'.

instance Integral a => Arbitrary (Expr a) where
  arbitrary = oneof [arbitrary_const, arbitrary_symbol]


-- arbitrary_const returns a random Const object by taking a random integer from -9 to 9 and wrapping it inside Const.
arbitrary_const :: Integral a => Gen (Expr a)
arbitrary_const = do
                    n <- elements ([-9, -8 .. 9] :: [Int])
                    return $ const' n

-- The constraint 'Integral a' in the signature is crucial since it allows us to use the const' smart constructor to create Const objects from randomly selected Int.
-- Note the use of 'do' to package a sequence of IO actions namely choosing a random integer and then returning the Const.
-- The '<-' is used to extract the chosen integer from its 'Gen' context.
-- The 'return' is a keyword from monad syntax which places the constructed Const inside the relevant context, in this case 'Integral a => Gen (Expr a)'. It guesses the context from, well, context.


-- Analogous to 'arbitrary_const' this definition, 'arbitrary_symbol', returns a Symbol object corresponding (randomly) to x, y or z.
-- Note that the signature reveals this to be a definition (and not a function). It corresponds to a randomly selected Expr.
arbitrary_symbol :: Gen (Expr a)
arbitrary_symbol = do
                    c <- elements ["x", "y", "z"]
                    return $ Symbol c



prop_Add_0 :: Expr Int -> Bool      -- A property of expressions is that adding zero to an expression should result in the same expression
prop_Add_0 e = e + z0 == e
    where types = e::(Expr Int)

prop_Mul_1 :: Expr Int -> Bool
prop_Mul_1 e = e * z1 == e
    where types = e::(Expr Int)