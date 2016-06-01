-- Defines a list of the Unit Tests that test aspects of Ordering of expressions

module UnitTests.Ordering (tests)
    where

import Prelude hiding ((^))     -- This allows us to use the ^ operator defined in CAS without collision with Prelude.^
import Test.HUnit(Test(TestCase, TestLabel))

import CAS
import UnitTests.Base


tests = [
            TestLabel "Order of Product Elements" $
                TestCase $ do

                    aE "test1" "(x * y)" $ show (x * y)
                    aE "test2" "(2 * x)" $ show (x * 2)
                    aE "test3" "(2 * x^2 * y)" $ show (2 * y * x^2)
                    aE "test4" "-(2 * x * y^2)" $ show (x * (-2) * y^2)
                    aE "test5" "(x * y * (y + z))" $ show (x * y * (y + z))
                    aE "test6" "(x * z^2 * (x + y))" $ show ((x + y) * z^2 * x)
                    aE "test7" "(x * z^2 * (x + y) * (y + z))" $ show ((y + z) * z^2 * (x + y) * x)
                    aE "test8" "(x * y^2 * (y + z)^3)" $ show ((y + z)^2 * y^2 * (y + z) * x)
                    aE "test9" "((y + z) * ((x * y) + 1))" $ show ((x * y + 1) * (y + z))
            ,

            TestLabel "Graded Reversed Lexical Order" $
                TestCase $ do

                    aE "test1"  EQ $ compare x x
                    aE "test2"  LT $ compare 2 x
                    aE "test3"  GT $ compare x y
                    aE "test4"  GT $ compare (x^2) x
                    aE "test5"  LT $ compare x (y^2)
                    aE "test6"  GT $ compare x (2 * y)
                    aE "test7"  LT $ compare (2 * y) (3 * x)
                    aE "test8"  EQ $ compare (2 * x) (2 * x)
                    aE "test9"  GT $ compare (x * y) (y * z)
                    aE "test10" LT $ compare (x * y) (x^2)
                    aE "test11" GT $ compare (x * y^2 * z) (x * y * z^2)
                    aE "test12" GT $ compare (x * y * z) (x * z^2)
                    aE "test13" EQ $ compare (x * y * z) (x * y * z)
                    aE "test14" EQ $ compare (x * y^2 * z) (x * y^2 * z)
                    aE "test15" LT $ compare x (2 * x)
                    aE "test16" GT $ compare (3 * x) x
                    aE "test17" LT $ compare x (-2 * x)             -- Negative constants shouldn't impact the order of terms
            ,

            TestLabel "Order of Added Elements" $
                TestCase $ do

                    aE "test1" "(x + 2)" $ show (x + 2)
                    aE "test2" "(x + y)" $ show (x + y)
                    aE "test3" "(x^2 + y)" $ show (x^2 + y)
                    aE "test4" "(y^2 + x)" $ show (x + y^2)
                    aE "test5" "(y + (2 * z) + 1)" $ show $ y + (1 + 2 * z)
                    aE "test6" "(x - y)" $ show (x - y)
                    aE "test7" "(x - y)" $ show (-y + x)
                    aE "test8" "(x/y + 1)" $ show (1 + x/y)
                    aE "test9" "(x + x/y)" $ show (x + x/y)
                    aE "test10" "(1/x + 1/y)" $ show (1/x + 1/y)
                    aE "test11" "(x/y + y/z)" $ show (x/y + y/z)
                    aE "test12" "(x/y + z/y)" $ show (x/y + z/y)
                    aE "test13" "(x^2/y + z^2/x)" $ show (x^2/y + z^2/x)
            ,

            TestLabel "Comparing expressions" $
                TestCase $ do

                    aE "test1" EQ $ compare x x
                    aE "test2" GT $ compare x y
                    aE "test3" LT $ compare y x
                    aE "test4" EQ $ compare (x + y) (y + x)
                    aE "test5" GT $ compare (x + y) (y + z)
                    aE "test6" GT $ compare (x - z - 2) (1 - y)
                    aE "test7" GT $ compare (x^2 + y^2) (z^2)
                    aE "test8" GT $ compare y (2*z)
                    aE "test9" LT $ compare z (3*y)
                    aE "test10" GT $ compare (x + 1) (y + 2)
                    aE "test11" GT $ compare x (-x)
                    aE "test12" GT $ compare (z + 1) (-z + 1)
                    aE "test13" GT $ compare (z^2) ((z + 1)^2)
        ]
