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
import CAS

main = runTestTT tests          -- In the main function we simply run the tests. So running the executable (TestCAS) will now cause the tests to be executed


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

            TestLabel "Testing Constants" $                     -- We use TestLabel to add a label to the TestCase which will be shown in case of failure
                TestCase $ do                                   -- Each TestCase contains a sequence of assertions inside a do construct

                    aE "test1" z2 z2                            -- IF this assertion fails both "Testing Constants" and "test1" will appear in the report
                    aE "test2" zm3 zm3
                    aB "test3" $ z4 > z3
                    aB "test4" $ z5 < z6
                    aB "test5" $ zm3 < zm2
                    aB "test6" $ zm7 > zm8
            ,                                                   -- This comma delimits the TestLabels inside the TestList list

            TestLabel "Adding Constants" $
                TestCase $ do

                    aE "test1" z5 (z2 + z3)
                    aE "test2" z4 (z7 + zm3)
                    aE "test3" zm4 (zm4 + z0)
        ]



-- Define shorthand utility functions for assertions

aE :: (Eq a, Show a) => String -> a -> a -> Assertion
aE = assertEqual

aB :: String -> Bool -> Assertion
aB = assertBool