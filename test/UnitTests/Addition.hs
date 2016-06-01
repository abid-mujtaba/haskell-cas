
-- Defines a list of the Unit Tests that test aspects of Addition

module UnitTests.Addition (tests)
    where

import Prelude hiding ((^))     -- This allows us to use the ^ operator defined in CAS without collision with Prelude.^
import Test.HUnit

import CAS
import UnitTests.Base


-- We add a label to the TestCase by using the TestLabel constructor.
-- The assertions are grouped together as a single TestCase
-- Since the assertions are IO () we use the 'do' keyword to group together a sequence of them
-- The first assertion that fails causes the entire TestCase to fail and the subsequent assertions are not tested
-- Each assertEqual call takes the format: aE <failure message> <expected value> <actual/tested value>

tests = [                                              -- We create a list of TestCases
            TestLabel "Adding similar products" $
                TestCase $ do

                    let e = x + y

                    aE "test1" (2 * e) (e + e)
                    aE "test2" (5 * e) ((2 * e) + (3 * e))
                    aE "test3" (3 * e) ((-2 * e) + (5 * e))
                    aE "test4" (-7 * e) ((-3 * e) + (4 * (-e)))
            ,

            TestLabel "Adding element to product of same element" $
                TestCase $ do

                    aE "test1" (3 * x) (x + (2 * x))
                    aE "test2" (-2 * x) (x + (-3 * x))
                    aE "test3" (3 * x * y) ((x * y) + (2 * x * y))
                    aE "test4" (3 * x^2) (x^2 + (2 * x^2))
            ,

            TestLabel "Subtracting equal expressions" $
                TestCase $ do

                    let e1 = -1 + x
--                    let e2 = 2 * (-x) / y
                    let e3 = -y + (2 * x * y)
                    let e4 = x + y
                    let e5 = -1 + e4
                    let e6 = e5 + z

                    aE "test2" 0 (x - x)
                    aE "test3" 0 (e1 - e1)
--                    aE "test4" 0 (e2 - e2)
                    aE "test5" 0 (e3 - e3)
                    aE "test6" 0 (e4 - e4)
                    aE "test7" 0 (e5 - e5)
                    aE "test8" 0 (e6 - e6)
            ,

            TestLabel "Additive Commutation" $
                TestCase $ do

                    let e2 = (1 + 2 * y)
                    let e3 = (2 + y)
                    let e4 = (1 + 2 * z)

                    let e5 = 9 * y * z^2
                    let e6 = z * (z + 1)^2

                    aE "test1" (x + y) (y + x)
                    aE "test2" (x + e2) (e2 + x)
                    aE "test3" (e3 + e4) (e4 + e3)
                    aE "test4" (e5 + e6) (e6 + e5)
        ]
